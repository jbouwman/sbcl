/* -*- Mode: C -*- */

/* Process-heap support that needs the mark-region collector's internals:
 * block claiming, the local (single heap) collection, the wholesale
 * root walk performed by a global collection, and the ownership
 * verifier.  Included at the end of mark-region.c. */

#ifdef LISP_FEATURE_SB_PROCESS_HEAPS

#include "thread.h"
#include "interrupt.h"
#include "lispregs.h"
#include "pseudo-atomic.h"
#include "fiber.h"

#define PH_OLD_GROWTH_FLOOR (1024 * 1024)
static page_index_t ph_free_page_hint;

/* --- Per-thread local collection state --- */

static _Atomic(int) ph_local_gcs_running, ph_local_gcs_peak;

uword_t process_heap_concurrency_peak(void)
{
    return atomic_load(&ph_local_gcs_peak);
}

static struct ph_local_gc *ph_thread_context(struct thread *th)
{
    struct extra_thread_data *ed = thread_extra_data(th);
    struct ph_local_gc *ctx = ed->local_gc;
    if (!ctx) {
        ctx = calloc(1, sizeof *ctx);
        gc_assert(ctx);
        ctx->wp_chain = WEAK_POINTER_CHAIN_END;
        hopscotch_create(&ctx->wobjects, HOPSCOTCH_HASH_FUN_DEFAULT, N_WORD_BYTES,
                         32 /* logical bin count */, 0 /* default range */);
        ed->local_gc = ctx;
    }
    return ctx;
}

void process_heap_thread_exit(struct thread *th)
{
    struct extra_thread_data *ed = thread_extra_data(th);
    struct ph_local_gc *ctx = ed->local_gc;
    if (!ctx) return;
    ed->local_gc = NULL;
    for (struct Qblock *b = ctx->free_blocks, *next; b; b = next) {
        next = b->next;
        free(b);
    }
    for (struct cons *c = ctx->private_chunks, *next; c; c = next) {
        next = (struct cons*)c->cdr;
        free(c);
    }
    hopscotch_destroy(&ctx->wobjects);
    free(ctx);
}

/* GC-private conses of a local collection come from malloc'd chunks
 * rather than from heap pages, whose allocator is not ours to use
 * while other threads run. */
#define PH_PRIVATE_CHUNK_BYTES 4096
bool process_heap_private_cons(uword_t car, uword_t cdr, uword_t *result)
{
    struct ph_local_gc *ctx = ph_local;
    if (!ctx) return false;
    struct cons *cons = ctx->private_recycle;
    if (cons) {
        ctx->private_recycle = (struct cons*)cons->cdr;
    } else {
        if (!ctx->private_avail) {
            struct cons *chunk = checked_malloc(PH_PRIVATE_CHUNK_BYTES);
            chunk->car = 0;
            chunk->cdr = (lispobj)ctx->private_chunks;
            ctx->private_chunks = chunk;
            ctx->private_avail = PH_PRIVATE_CHUNK_BYTES / sizeof (struct cons) - 1;
        }
        cons = ctx->private_chunks + ctx->private_avail--;
    }
    cons->car = car;
    cons->cdr = cdr;
    *result = (uword_t)cons;
    return true;
}

bool process_heap_private_free(struct cons *list)
{
    struct ph_local_gc *ctx = ph_local;
    if (!ctx) return false;
    struct cons *head = list;
    while (list->cdr) list = (struct cons*)list->cdr;
    list->cdr = (lispobj)ctx->private_recycle;
    ctx->private_recycle = head;
    return true;
}

bool process_heap_dispose_private(void)
{
    struct ph_local_gc *ctx = ph_local;
    if (!ctx) return false;
    /* Keep the chunks for the next collection; forget the conses. */
    ctx->private_recycle = NULL;
    ctx->private_avail = 0;
    struct cons *keep = ctx->private_chunks;
    if (keep) {
        for (struct cons *c = (struct cons*)keep->cdr, *next; c; c = next) {
            next = (struct cons*)c->cdr;
            free(c);
        }
        keep->cdr = 0;
        ctx->private_avail = PH_PRIVATE_CHUNK_BYTES / sizeof (struct cons) - 1;
    }
    return true;
}

/* --- Blocks ---
 *
 * Ownership has the granularity of a block (PH_BLOCK_BYTES) so that
 * small heaps cost a few kilobytes rather than a few pages.  A page
 * whose blocks belong to process heaps is a "process page"; the global
 * allocator never touches one, and free blocks on it are available to
 * any heap.  A heap's allocation regions and objects never cross from
 * one of its runs of blocks into another owner's, which is what lets
 * heaps that share a page allocate and collect independently. */

static inline line_index_t block_to_line(block_index_t b) { return b * PH_BLOCK_LINES; }

/* First block after B that is not owned by ID, at most LIMIT. */
static inline block_index_t ph_run_end(block_index_t b, block_index_t limit, uint32_t id)
{
    while (b < limit && block_owner[b] == id) b++;
    return b;
}

/* Iterate over the maximal runs [B, E) of blocks owned by H on page P. */
#define PH_FOR_RUNS(h, p, b, e)                                             \
    for (block_index_t b = page_to_block(p), e,                             \
             b##_limit = page_to_block((p) + 1);                            \
         b < b##_limit                                                      \
             && ((block_owner[b] == (h)->id)                                \
                 ? ((e = ph_run_end(b, b##_limit, (h)->id)), 1)             \
                 : ((e = b + 1), 1));                                       \
         b = e)                                                             \
        if (block_owner[b] == (h)->id)

static bool ph_heap_on_page(struct process_heap *h, page_index_t p)
{
    for (block_index_t b = page_to_block(p), e = b + PH_BLOCKS_PER_PAGE; b < e; b++)
        if (block_owner[b] == h->id) return true;
    return false;
}

static bool ph_page_has_free_block(page_index_t p)
{
    for (block_index_t b = page_to_block(p), e = b + PH_BLOCKS_PER_PAGE; b < e; b++)
        if (!block_owner[b]) return true;
    return false;
}

static bool ph_page_unowned(page_index_t p)
{
    for (block_index_t b = page_to_block(p), e = b + PH_BLOCKS_PER_PAGE; b < e; b++)
        if (block_owner[b]) return false;
    return true;
}

static uword_t ph_used_bytes_in_lines(line_index_t first, line_index_t end)
{
    uword_t n = 0;
    for (line_index_t l = first; l < end; l++) if (line_bytemap[l]) n++;
    return n * LINE_SIZE;
}

static bool ph_lines_all_free(line_index_t first, line_index_t end)
{
    return ph_used_bytes_in_lines(first, end) == 0;
}

/* Bound used by the allocator's medium path: a region on a process page
 * does not grow on its own, since claiming lines there must be
 * accounted for under the page table lock. */
static line_index_t process_heap_region_limit(struct alloc_region *region, line_index_t page_end)
{
    page_index_t p = find_page_index(region->start_addr);
    if (p < 0 || !ph_process_page[p]) return page_end;
    return address_line(region->end_addr);
}

/* --- Partial process pages: those with at least one free block --- */

struct ph_page_list { page_index_t *pages; sword_t count, capacity; };
static struct ph_page_list ph_partial[8];   /* indexed by page type */

static void ph_note_partial(page_index_t p)
{
    if (ph_page_partial[p]) return;
    struct ph_page_list *l = &ph_partial[page_table[p].type & PAGE_TYPE_MASK];
    if (l->count == l->capacity) {
        sword_t cap = l->capacity ? 2 * l->capacity : 64;
        page_index_t *np = realloc(l->pages, cap * sizeof *np);
        gc_assert(np);
        l->pages = np;
        l->capacity = cap;
    }
    l->pages[l->count++] = p;
    ph_page_partial[p] = 1;
}

/* Find a run of at least NEED free blocks on a process page of TYPE.
 * Entries that no longer qualify are dropped as they are met. */
static page_index_t ph_find_free_run(int type, int need, int want,
                                     block_index_t *first, int *count)
{
    struct ph_page_list *l = &ph_partial[type];
    for (sword_t i = 0; i < l->count; ) {
        page_index_t p = l->pages[i];
        bool stale = !ph_page_partial[p] || !ph_process_page[p] || page_free_p(p)
            || page_single_obj_p(p) || (page_table[p].type & PAGE_TYPE_MASK) != type;
        int best = 0;
        block_index_t best_start = -1;
        if (!stale) {
            block_index_t b0 = page_to_block(p), bend = b0 + PH_BLOCKS_PER_PAGE;
            for (block_index_t b = b0; b < bend; ) {
                if (block_owner[b]) { b++; continue; }
                block_index_t e = ph_run_end(b, bend, 0);
                if (e - b > best) best = e - b, best_start = b;
                if (best >= want) break;
                b = e;
            }
        }
        if (stale || best == 0) {
            ph_page_partial[p] = 0;
            l->pages[i] = l->pages[--l->count];
            continue;
        }
        if (best >= need) {
            *first = best_start;
            *count = best < want ? best : want;
            return p;
        }
        i++;
    }
    return -1;
}

/* --- Page bookkeeping --- */

static void ph_remove_page_at(struct process_heap *h, sword_t i)
{
    h->pages[i] = h->pages[--h->npages];
}

static void ph_reset_page(page_index_t p)
{
    set_page_need_to_zero(p, 1);
    set_page_type(page_table[p], FREE_PAGE_FLAG);
    page_table[p].scan_start_offset_ = 0;
    page_table[p].gen = 0;
    set_page_bytes_used(p, 0);
    gc_page_pins[p] = 0;
    ph_process_page[p] = 0;
    ph_page_partial[p] = 0;
    memset(block_owner + page_to_block(p), 0, PH_BLOCKS_PER_PAGE * sizeof *block_owner);
    memset(line_bytemap + page_to_line(p), 0, LINES_PER_PAGE);
    memset((unsigned char*)allocation_bitmap + page_to_line(p), 0, LINES_PER_PAGE);
    memset((unsigned char*)mark_bitmap + page_to_line(p), 0, LINES_PER_PAGE);
    if (p < ph_free_page_hint) ph_free_page_hint = p;
}

/* Give back the blocks [B, E) of a small page: their lines are already
 * free.  Caller holds the page table lock. */
static void ph_release_blocks(struct process_heap *h, block_index_t b, block_index_t e)
{
    line_index_t first = block_to_line(b), end = block_to_line(e);
    memset(line_bytemap + first, 0, end - first);
    memset((unsigned char*)allocation_bitmap + first, 0, end - first);
    memset((unsigned char*)mark_bitmap + first, 0, end - first);
    memset(block_owner + b, 0, (e - b) * sizeof *block_owner);
    h->nblocks -= e - b;
}

/* Called after H gave up blocks on small page P: free the page if no
 * heap owns any of it, else remember it has room. */
static void ph_settle_page(page_index_t p)
{
    if (ph_page_unowned(p)) {
        gc_assert(page_bytes_used(p) == 0);
        ph_reset_page(p);
    } else if (ph_page_has_free_block(p)) {
        ph_note_partial(p);
    }
}

static void ph_dump_page(page_index_t p)
{
    fprintf(stderr, "page #%d type %x used %d process %d partial %d\n", (int)p,
            page_table[p].type, page_bytes_used(p), ph_process_page[p], ph_page_partial[p]);
    for (block_index_t b = page_to_block(p), e = b + PH_BLOCKS_PER_PAGE; b < e; b++) {
        int nonzero = 0, fresh = 0, marked = 0, dead = 0;
        for (line_index_t l = block_to_line(b); l < block_to_line(b + 1); l++) {
            unsigned char c = line_bytemap[l];
            if (c) nonzero++;
            if (IS_FRESH(c)) fresh++;
            if (IS_MARKED(c)) marked++;
            if (UNFRESHEN_GEN(c) == ENCODE_GEN(gen_to_collect())) dead++;
        }
        fprintf(stderr, "  block %ld owner %u nonzero %d fresh %d marked %d dead %d\n",
                (long)(b - page_to_block(p)), block_owner[b], nonzero, fresh, marked, dead);
    }
}

static void ph_page_decrement(page_index_t p, uword_t bytes)
{
    if (page_bytes_used(p) < bytes) {
        ph_dump_page(p);
        lose("process heap page #%d: decrement of %lu with %d used",
             (int)p, (unsigned long)bytes, page_bytes_used(p));
    }
    set_page_bytes_used(p, page_bytes_used(p) - bytes);
    bytes_allocated -= bytes;
    generations[0].bytes_allocated -= bytes;
}

/* H's block count and page list must agree with the block table.
 * Caller holds the page table lock. */
void process_heap_check_blocks(struct process_heap *h, const char *where)
{
    uword_t owned = 0, unlisted = 0, duplicates = 0;
    block_index_t nblocks = (block_index_t)next_free_page * PH_BLOCKS_PER_PAGE;
    for (block_index_t b = 0; b < nblocks; b++) {
        if (block_owner[b] != h->id) continue;
        owned++;
        page_index_t p = b / PH_BLOCKS_PER_PAGE;
        bool listed = false;
        for (sword_t i = 0; i < h->npages; i++) if (h->pages[i] == p) { listed = true; break; }
        if (!listed) {
            unlisted++;
            fprintf(stderr, "ph CHECK %s: heap %u owns block %ld of unlisted page %ld (type %x single %d)\n",
                    where, h->id, (long)(b - page_to_block(p)), (long)p,
                    page_table[p].type, page_single_obj_p(p));
        }
    }
    for (sword_t i = 0; i < h->npages; i++)
        for (sword_t j = i + 1; j < h->npages; j++)
            if (h->pages[i] == h->pages[j]) duplicates++;
    if (owned != h->nblocks || unlisted || duplicates) {
        fprintf(stderr, "ph CHECK %s: heap %u nblocks %lu owned %lu unlisted %lu duplicate pages %lu npages %ld\n",
                where, h->id, (unsigned long)h->nblocks, (unsigned long)owned,
                (unsigned long)unlisted, (unsigned long)duplicates, (long)h->npages);
        lose("process heap block accounting is wrong at %s", where);
    }
}

/* Return every block of H to the free pool. Caller holds the page table
 * lock; H is not installed anywhere. */
void process_heap_free_pages_locked(struct process_heap *h)
{
    /* The byte counts assume closed regions: an open region's run has
     * free lines that were claimed but not yet marked. */
    ensure_region_closed(&h->parked_mixed, PAGE_TYPE_MIXED);
    ensure_region_closed(&h->parked_cons, PAGE_TYPE_CONS);
    for (sword_t i = 0; i < h->npages; i++) {
        page_index_t p = h->pages[i];
        if (page_single_obj_p(p)) {
            ph_page_decrement(p, page_bytes_used(p));
            h->nblocks -= PH_BLOCKS_PER_PAGE;
            ph_reset_page(p);
        } else {
            PH_FOR_RUNS(h, p, b, e) {
                ph_page_decrement(p, ph_used_bytes_in_lines(block_to_line(b), block_to_line(e)));
                ph_release_blocks(h, b, e);
            }
            ph_settle_page(p);
        }
    }
    gc_assert(h->nblocks == 0);
    h->npages = 0;
    h->bytes_allocated = 0;
    h->bytes_live = 0;
    h->bytes_since_gc = 0;
}

static page_index_t ph_find_free_page(void)
{
    page_index_t p = find_free_page(ph_free_page_hint, page_table_pages);
    if (p == -1 && ph_free_page_hint > 0)
        p = find_free_page(0, ph_free_page_hint);
    if (p != -1) ph_free_page_hint = p + 1;
    return p;
}

static void ph_own_blocks(struct process_heap *h, block_index_t first, int n)
{
    for (int i = 0; i < n; i++) block_owner[first + i] = h->id;
    h->nblocks += n;
}

static void ph_account_claim(struct process_heap *h, page_index_t p, uword_t claimed)
{
    set_page_bytes_used(p, page_bytes_used(p) + claimed);
    bytes_allocated += claimed;
    generations[0].bytes_allocated += claimed;
    h->bytes_allocated += claimed;
    h->bytes_since_gc += claimed;
}

/* Claim lines for a small-object region: free lines inside runs the
 * heap already owns (there can only be some after a local GC), then
 * free blocks on a process page, then a free page.  Heaps claim bigger
 * runs as they grow, up to whole pages.  What a region covers is what
 * gets accounted for; page byte counts equal the used lines on process
 * pages at all times.  Caller holds the page table lock. */
static bool ph_claim_small(struct process_heap *h, sword_t nbytes,
                           struct alloc_region *region, int page_type)
{
    int type = page_type & PAGE_TYPE_MASK;
    for (sword_t i = h->reuse_cursor; i < h->npages; i++) {
        page_index_t p = h->pages[i];
        if (page_single_obj_p(p) || (page_table[p].type & PAGE_TYPE_MASK) != type)
            continue;
        PH_FOR_RUNS(h, p, b, e) {
            line_index_t first = block_to_line(b), end = block_to_line(e);
            if (try_allocate_small(nbytes, region, first, end)) {
                ph_account_claim(h, p, addr_diff(region->end_addr, region->start_addr));
                h->reuse_cursor = i;
                return true;
            }
        }
    }
    h->reuse_cursor = h->npages;

    int need = ALIGN_UP(nbytes, PH_BLOCK_BYTES) / PH_BLOCK_BYTES;
    int want = h->nblocks / 2;
    if (want < need) want = need;
    if (want > PH_BLOCKS_PER_PAGE) want = PH_BLOCKS_PER_PAGE;

    block_index_t first;
    int count;
    page_index_t p = ph_find_free_run(type, need, want, &first, &count);
    if (p != -1) {
        bool listed = ph_heap_on_page(h, p);
        ph_own_blocks(h, first, count);
        if (!try_allocate_small(nbytes, region, block_to_line(first), block_to_line(first + count)))
            lose("free blocks on page %d have no free lines", (int)p);
        if (!listed) process_heap_add_page(h, p);
        ph_account_claim(h, p, count * PH_BLOCK_BYTES);
        if (!ph_page_has_free_block(p)) ph_page_partial[p] = 0;
        return true;
    }

    p = ph_find_free_page();
    if (p == -1) return false;
    prepare_pages(1, p, p, 0, 0);
    /* Process pages never carry OPEN_REGION_PAGE_FLAG: several heaps may
     * have regions open on one page. */
    set_page_type(page_table[p], type);
    page_table[p].gen = 0;
    set_page_scan_start_offset(p, 0);
    set_page_bytes_used(p, 0);
    ph_process_page[p] = 1;
    if (p + 1 > next_free_page) next_free_page = p + 1;
    first = page_to_block(p);
    ph_own_blocks(h, first, want);
    if (!try_allocate_small(nbytes, region, block_to_line(first), block_to_line(first + want)))
        lose("free page %d has no free lines", (int)p);
    process_heap_add_page(h, p);
    ph_account_claim(h, p, want * PH_BLOCK_BYTES);
    if (want < PH_BLOCKS_PER_PAGE) ph_note_partial(p);
    return true;
}

static page_index_t ph_claim_large(struct process_heap *h, sword_t nbytes,
                                   int page_type)
{
    struct allocator_state state = { 0, true };
    uword_t largest_hole = 0;
    page_index_t first = try_allocate_large(nbytes, page_type, 0, &state,
                                            page_table_pages, &largest_hole);
    if (first == -1) return -1;
    page_index_t npages = ALIGN_UP(nbytes, GENCGC_PAGE_BYTES) / GENCGC_PAGE_BYTES;
    for (page_index_t p = first; p < first + npages; p++) {
        ph_process_page[p] = 1;
        ph_own_blocks(h, page_to_block(p), PH_BLOCKS_PER_PAGE);
        process_heap_add_page(h, p);
    }
    h->bytes_allocated += nbytes;
    h->bytes_since_gc += nbytes;
    return first;
}

static void ph_request_collection(struct thread *th, struct process_heap *h)
{
    if (read_TLS(GC_INHIBIT, th) != NIL) return;
    if (get_pseudo_atomic_interrupted(th)) return;
    set_pseudo_atomic_interrupted(th);
    maybe_save_gc_mask_and_block_deferrables(0);
}

lispobj *process_heap_alloc_slow(struct thread *th, struct process_heap *h,
                                 struct alloc_region *region,
                                 sword_t nbytes, int page_type)
{
    gc_assert(h->installed_on == th);
    ensure_region_closed(region, page_type);

    /* Arrange for a local collection at the end of this pseudo-atomic
     * section when the heap has grown enough since the last one. */
    if (h->gc_pending) {
        ph_request_collection(th, h);
    } else if (h->gc_threshold && h->bytes_since_gc >= h->gc_threshold) {
        h->gc_pending = 1;
        ph_request_collection(th, h);
    }

    bool largep = nbytes >= LARGE_OBJECT_SIZE && page_type != PAGE_TYPE_CONS;
    uword_t claim = largep ? ALIGN_UP(nbytes, GENCGC_PAGE_BYTES) : ALIGN_UP(nbytes, PH_BLOCK_BYTES);
    if (h->hard_limit && h->bytes_allocated + claim > h->hard_limit)
        process_heap_exhausted(h, nbytes);

    void *new_obj;
    acquire_gc_page_table_lock();
    if (largep) {
        page_index_t p = ph_claim_large(h, nbytes, page_type);
        if (p == -1) gc_heap_exhausted_error_or_lose(0, nbytes);
        new_obj = page_address(p);
        set_allocation_bit_mark(new_obj);
        release_gc_page_table_lock();
        memset(new_obj, 0, nbytes);
    } else {
        if (!ph_claim_small(h, nbytes, region, page_type))
            gc_heap_exhausted_error_or_lose(0, nbytes);
        release_gc_page_table_lock();
        new_obj = region->start_addr;
        memset(new_obj, 0, addr_diff(region->end_addr, new_obj));
    }
    return new_obj;
}

/* --- Walking a heap's objects --- */

/* The object starts in the lines [FIRST, END), which must be materialized. */
#define PH_FOR_OBJECTS_IN_LINES(where, first, end)                                  \
    for (lispobj *where = ph_first_object(first, end), *where##_limit = (lispobj*)line_address(end); \
         where; where = next_object(where, object_size(where), where##_limit))

static lispobj *ph_first_object(line_index_t first, line_index_t end)
{
    lispobj *where = (lispobj*)line_address(first), *limit = (lispobj*)line_address(end);
    return allocation_bit_marked(where) ? where : next_object(where, 0, limit);
}

/* --- Allocation bits --- */

/* Fresh lines have no allocation bits yet; the collector computes them
 * lazily for ambiguous pointers.  A process heap's objects must be
 * enumerable, so compute them for every fresh span of every run.  The
 * spans are bounded by the run, not the page: a neighbouring heap may be
 * allocating into its own fresh lines right now. */
static void ph_materialize_lines(page_index_t p, line_index_t first, line_index_t end)
{
    unsigned char *allocations = (unsigned char*)allocation_bitmap;
    bool cons_page = (page_table[p].type & PAGE_TYPE_MASK) == PAGE_TYPE_CONS;
    for (line_index_t l = first; l < end; ) {
        if (!IS_FRESH(line_bytemap[l])) { l++; continue; }
        line_index_t s = l;
        for (; l < end && IS_FRESH(line_bytemap[l]); l++)
            line_bytemap[l] = UNFRESHEN_GEN(line_bytemap[l]);
        if (cons_page) {
            memset(allocations + s, 0xFF, l - s);
        } else {
            lispobj *where = (lispobj*)line_address(s), *limit = (lispobj*)line_address(l);
            while (where < limit) {
                if (*where != 0) set_allocation_bit_mark(where);
                where += object_size(where);
            }
        }
    }
}

void process_heap_materialize(struct process_heap *h)
{
    for (sword_t i = 0; i < h->npages; i++) {
        page_index_t p = h->pages[i];
        if (page_single_obj_p(p)) continue;
        PH_FOR_RUNS(h, p, b, e)
            ph_materialize_lines(p, block_to_line(b), block_to_line(e));
    }
}

static void ph_close_installed_regions(struct thread *th)
{
    acquire_gc_page_table_lock();
    ensure_region_closed(&th->mixed_tlab, PAGE_TYPE_MIXED);
    ensure_region_closed(&th->cons_tlab, PAGE_TYPE_CONS);
    release_gc_page_table_lock();
}

/* --- Global GC integration --- */

/* Close every parked allocation region so that no page is left in the
 * open state, then make every process object enumerable. */
void process_heaps_before_global_gc(void)
{
    struct thread *th;
    for_each_thread(th) {
        struct extra_thread_data *ed = thread_extra_data(th);
        if (ed->current_heap) {
            ensure_region_closed(&ed->saved_mixed_tlab, PAGE_TYPE_MIXED);
            ensure_region_closed(&ed->saved_cons_tlab, PAGE_TYPE_CONS);
        }
    }
    for (uint32_t i = 0; i < process_heap_table_size; i++) {
        struct process_heap *h = process_heap_table[i];
        if (!h || h->state != PROCESS_HEAP_LIVE) continue;
        if (!h->installed_on) {
            ensure_region_closed(&h->parked_mixed, PAGE_TYPE_MIXED);
            ensure_region_closed(&h->parked_cons, PAGE_TYPE_CONS);
        }
        process_heap_materialize(h);
    }
}

/* --- Outgoing global-root summaries ---
 *
 * A global collection must keep alive whatever process objects refer
 * to, but walking every process object makes its cost grow with the
 * total size of the process heaps.  Instead each local collection
 * records the global objects it marks as leaves: after it, every global
 * object referred to from the heap's old generation is in the summary,
 * or the referring object sits on a card marked since.  A global
 * collection then marks the summary, and walks only the young
 * generation and the old objects on marked cards.  A full local
 * collection rebuilds the summary; a minor one adds to it, so stale
 * entries can only retain global objects until the next full one.
 *
 * The summary is a hash set, filled by the one thread tracing the
 * heap. */

static inline uword_t ph_outgoing_slot(lispobj object, uword_t mask)
{
    return ((object >> N_LOWTAG_BITS) * 0x9E3779B97F4A7C15ull >> 24) & mask;
}

static void ph_outgoing_grow(struct process_heap *h)
{
    uword_t old_mask = h->outgoing_mask, mask = h->outgoing ? 2 * old_mask + 1 : 255;
    lispobj *table = calloc(mask + 1, sizeof *table);
    gc_assert(table);
    if (h->outgoing) {
        for (uword_t i = 0; i <= old_mask; i++) {
            lispobj object = h->outgoing[i];
            if (!object) continue;
            uword_t j = ph_outgoing_slot(object, mask);
            while (table[j]) j = (j + 1) & mask;
            table[j] = object;
        }
        free(h->outgoing);
    }
    h->outgoing = table;
    h->outgoing_mask = mask;
}

static void process_heap_note_outgoing(lispobj object)
{
    struct process_heap *h = ph_local->heap;
    if (!h->outgoing || 2 * (h->noutgoing + 1) > h->outgoing_mask + 1)
        ph_outgoing_grow(h);
    uword_t mask = h->outgoing_mask, i = ph_outgoing_slot(object, mask);
    while (h->outgoing[i]) {
        if (h->outgoing[i] == object) return;
        i = (i + 1) & mask;
    }
    h->outgoing[i] = object;
    h->noutgoing++;
}

static void ph_outgoing_clear(struct process_heap *h)
{
    if (h->outgoing) memset(h->outgoing, 0, (h->outgoing_mask + 1) * sizeof (lispobj));
    h->noutgoing = 0;
}

/* Roots of a global collection held by H: its summary, its young
 * generation, and the old objects on marked cards.  The card marks are
 * left alone; they belong to H's own collections.  The objects walked
 * may be dead, so their edges to global memory are ambiguous (see
 * mark()). */
static void ph_trace_heap_roots(struct process_heap *h)
{
    uword_t walked = 0;
    /* Reference checking wants to see every edge, summary or not. */
    bool walk_all = process_heap_check_refs;
    /* Summary entries are exact: they were live objects when recorded
     * and the summary has kept them alive since. */
    source_object = NULL;
    if (h->outgoing)
        for (uword_t i = 0; i <= h->outgoing_mask; i++)
            if (h->outgoing[i]) mark(h->outgoing[i], NULL, SOURCE_NORMAL);
    for (sword_t j = 0; j < h->npages; j++) {
        page_index_t p = h->pages[j];
        if (page_single_obj_p(p)) {
            if (!page_starts_contiguous_block_p(p)) continue;
            lispobj *obj = (lispobj*)page_address(p);
            bool interesting = walk_all || page_table[p].gen == 0;
            if (!interesting) {
                uword_t nbytes = object_size(obj) * N_WORD_BYTES;
                page_index_t last = p + (nbytes - 1) / GENCGC_PAGE_BYTES;
                for (page_index_t q = p; q <= last && !interesting; q++)
                    for (int c = 0, card = page_to_card_index(q); c < CARDS_PER_PAGE; c++, card++)
                        if (card_dirtyp(card)) { interesting = 1; break; }
            }
            if (interesting) {
                source_object = obj;
                trace_object(compute_lispobj(obj));
                walked++;
            }
        } else {
            unsigned char young = ENCODE_GEN(0);
            PH_FOR_RUNS(h, p, b, e) {
                line_index_t first = block_to_line(b), end = block_to_line(e);
                lispobj *spanning = NULL;
                for (line_index_t l = first; l < end; l++) {
                    unsigned char line = UNFRESHEN_GEN(line_bytemap[l]);
                    if (!line) continue;
                    bool old = UNMARK_GEN(line) != young;
                    if (old && !walk_all
                        && gc_card_mark[addr_to_card_index(line_address(l))] == CARD_UNMARKED)
                        continue;
                    lispobj *start = (lispobj*)line_address(l), *limit = start + LINE_SIZE / N_WORD_BYTES;
                    /* An old object that starts earlier and reaches into
                     * this marked card. */
                    if (old && !allocation_bit_marked(start)) {
                        lispobj *before = find_object((uword_t)start, (uword_t)line_address(first));
                        if (before && before != spanning) {
                            source_object = spanning = before;
                            trace_object(compute_lispobj(before));
                            walked++;
                        }
                    }
                    for (lispobj *where = allocation_bit_marked(start) ? start : next_object(start, 0, limit);
                         where; where = next_object(where, object_size(where), limit)) {
                        source_object = where;
                        trace_object(compute_lispobj(where));
                        walked++;
                        /* A young object spanning several lines is
                         * traced from its first line only. */
                        if (where + object_size(where) > limit) {
                            spanning = where;
                            l = address_line(where + object_size(where) - 1);
                            break;
                        }
                    }
                }
            }
        }
    }
    h->global_root_objects = walked;
}

void process_heaps_trace_roots(void)
{
    for (uint32_t i = 0; i < process_heap_table_size; i++) {
        struct process_heap *h = process_heap_table[i];
        if (!h || h->state != PROCESS_HEAP_LIVE) continue;
        ph_trace_heap_roots(h);
    }
    source_object = NULL;
}

/* --- Local collection --- */

#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
extern void visit_context_registers(void (*proc)(os_context_register_t, void*),
                                    os_context_t *context, void*);
static void ph_preserve_register(os_context_register_t word,
                                 __attribute__((unused)) void *arg)
{
    mr_preserve_ambiguous(word);
}
#elif defined reg_LINK_RETURN
static int ph_boxed_registers[] = BOXED_REGISTERS;
#endif

static void ph_scan_thread_roots(struct thread *th, void *approx_sp)
{
    lispobj pin_list = read_TLS(PINNED_OBJECTS, th);
    for ( ; pin_list != NIL ; pin_list = CONS(pin_list)->cdr )
        mr_preserve_object(CONS(pin_list)->car);

    int ncontexts = fixnum_value(read_TLS(FREE_INTERRUPT_CONTEXT_INDEX, th));
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
    /* Conservative: the Lisp frames that called us, and everything
     * above them, live on this stack. */
    for (lispobj *ptr = approx_sp; ptr < th->control_stack_end; ptr++) {
        lispobj word = *ptr;
        if (word >= BACKEND_PAGE_BYTES) mr_preserve_ambiguous(word);
    }
    for (int i = ncontexts - 1; i >= 0; i--)
        visit_context_registers(ph_preserve_register,
                                nth_interrupt_context(i, th), 0);
#else
    /* Precise: the control stack holds only descriptors. */
    for (lispobj *ptr = th->control_stack_start;
         ptr < access_control_stack_pointer(th); ptr++) {
        lispobj word = *ptr;
        if (is_lisp_pointer(word)) mr_preserve_object(word);
    }
# ifdef reg_LINK_RETURN
    for (int i = ncontexts - 1; i >= 0; i--) {
        os_context_t *context = nth_interrupt_context(i, th);
        for (unsigned r = 0; r < sizeof ph_boxed_registers / sizeof (int); r++) {
            lispobj word = *os_context_register_addr(context, ph_boxed_registers[r]);
            if (is_lisp_pointer(word)) mr_preserve_object(word);
        }
    }
# endif
#endif
    (void)approx_sp;

    scav_binding_stack((lispobj*)th->binding_stack_start,
                       (lispobj*)get_binding_stack_pointer(th),
                       mr_preserve_object);
    lispobj *from = &th->lisp_thread;
    lispobj *to = (lispobj*)(SymbolValue(FREE_TLS_INDEX, 0) + (char*)th);
    mr_preserve_range(from, to - from);

#ifdef LISP_FEATURE_SB_FIBER
    /* Suspended fibers on this thread may hold pointers into the heap
     * (mark() ignores anything that isn't in the target heap). */
    gc_scan_fiber_stacks(th);
    gc_scav_fiber_binding_stacks(th);
#endif
}

/* Move every line and large page of H in generation FROM to generation TO. */
static void ph_raise_generation(struct process_heap *h,
                                generation_index_t from, generation_index_t to)
{
    unsigned char from_line = ENCODE_GEN(from), to_line = ENCODE_GEN(to);
    for (sword_t i = 0; i < h->npages; i++) {
        page_index_t p = h->pages[i];
        if (page_single_obj_p(p)) {
            if (page_table[p].gen == from) page_table[p].gen = to;
        } else {
            PH_FOR_RUNS(h, p, b, e)
                for (line_index_t l = block_to_line(b), end = block_to_line(e); l < end; l++)
                    if (UNFRESHEN_GEN(line_bytemap[l]) == from_line)
                        line_bytemap[l] = COPY_MARK(line_bytemap[l], to_line);
        }
    }
}

/* Large objects of the generation being collected get their byte counts
 * back only if they are marked. Caller holds the page table lock. */
static void ph_reset_large_pages(struct process_heap *h, generation_index_t gen)
{
    for (sword_t i = 0; i < h->npages; i++) {
        page_index_t p = h->pages[i];
        if (page_single_obj_p(p) && page_table[p].gen == gen) {
            uword_t used = page_bytes_used(p);
            bytes_allocated -= used;
            generations[0].bytes_allocated -= used;
            set_page_bytes_used(p, 0);
        }
    }
}

/* Roots of a minor collection held by the heap's old generation: scan
 * the marked cards of its runs, as the global collector does for the
 * older generations of the global heap. */
static void ph_scan_old_cards(struct process_heap *h)
{
    uword_t prefixes = 0;
    for (sword_t i = 0; i < h->npages; i++) {
        page_index_t p = h->pages[i];
        if (page_single_obj_p(p)) {
            scavenge_root_page(p, &prefixes);
        } else {
            PH_FOR_RUNS(h, p, b, e)
                scavenge_root_lines(p, block_to_line(b), block_to_line(e), &prefixes);
        }
    }
    dirty_generation_source = 0;
    dirty = 0;
}

static uword_t ph_old_bytes(struct process_heap *h)
{
    uword_t total = 0;
    unsigned char young = ENCODE_GEN(0);
    for (sword_t i = 0; i < h->npages; i++) {
        page_index_t p = h->pages[i];
        if (page_single_obj_p(p)) {
            if (page_table[p].gen > 0) total += page_bytes_used(p);
        } else {
            PH_FOR_RUNS(h, p, b, e)
                for (line_index_t l = block_to_line(b), end = block_to_line(e); l < end; l++) {
                    unsigned char line = UNFRESHEN_GEN(line_bytemap[l]);
                    if (line && line != young) total += LINE_SIZE;
                }
        }
    }
    return total;
}

/* After a collection every survivor is old and every edge out of the
 * heap is in the summary, so the card marks of H's blocks carry no
 * information: clear them, so that a global collection walks only the
 * objects stored into since.  The minor card scan resets the cards of
 * the lines it looks at, but not those of a young large object, and a
 * full collection scans no cards at all.  Sticky marks belong to the
 * global collector and are left alone.  Caller holds the page table
 * lock. */
static void ph_clear_cards(struct process_heap *h)
{
    for (sword_t i = 0; i < h->npages; i++) {
        page_index_t p = h->pages[i];
        if (page_single_obj_p(p)) {
            for (int c = 0, card = page_to_card_index(p); c < CARDS_PER_PAGE; c++, card++)
                if (gc_card_mark[card] != STICKY_MARK) gc_card_mark[card] = CARD_UNMARKED;
        } else {
            /* Cards are as large as lines. */
            PH_FOR_RUNS(h, p, b, e) {
                line_index_t first = block_to_line(b), end = block_to_line(e);
                int card = addr_to_card_index(line_address(first));
                for (line_index_t l = first; l < end; l++, card++)
                    if (gc_card_mark[card] != STICKY_MARK) gc_card_mark[card] = CARD_UNMARKED;
            }
        }
    }
}

/* Sweep the generation being collected out of H's blocks, giving back
 * the blocks that end up empty.  Caller holds the page table lock. */
static void ph_sweep(struct process_heap *h, generation_index_t gen)
{
    for (sword_t i = h->npages - 1; i >= 0; i--) {
        page_index_t p = h->pages[i];
        if (page_single_obj_p(p)) {
            if (page_table[p].gen != gen) continue;
            uword_t used = page_bytes_used(p);
            if (used == 0) {
                h->nblocks -= PH_BLOCKS_PER_PAGE;
                ph_reset_page(p);
                ph_remove_page_at(h, i);
            } else {
                bytes_allocated += used;
                generations[0].bytes_allocated += used;
                if (page_starts_contiguous_block_p(p))
                    mark_bitmap[mark_bitmap_word_index(page_address(p))] = 0;
            }
        } else {
            PH_FOR_RUNS(h, p, b, e) {
                line_index_t first = block_to_line(b), end = block_to_line(e);
                ph_page_decrement(p, count_dead_lines(first, end));
                sweep_small_lines(first, end);
                /* Give back the blocks of the run that are now empty. */
                for (block_index_t blk = b; blk < e; ) {
                    block_index_t free_end = blk;
                    while (free_end < e
                           && ph_lines_all_free(block_to_line(free_end), block_to_line(free_end + 1)))
                        free_end++;
                    if (free_end > blk) {
                        ph_release_blocks(h, blk, free_end);
                        blk = free_end;
                    } else {
                        blk++;
                    }
                }
            }
            gc_page_pins[p] = 0;
            bool still_here = ph_heap_on_page(h, p);
            ph_settle_page(p);
            if (!still_here) ph_remove_page_at(h, i);
        }
    }
    uword_t total = 0;
    for (sword_t i = 0; i < h->npages; i++) {
        page_index_t p = h->pages[i];
        if (page_single_obj_p(p)) {
            total += page_bytes_used(p);
        } else {
            PH_FOR_RUNS(h, p, b, e)
                total += ph_used_bytes_in_lines(block_to_line(b), block_to_line(e));
        }
    }
    h->bytes_allocated = total;
    h->bytes_live = total;
    h->bytes_since_gc = 0;
    h->reuse_cursor = 0;
}

/* Collect H.  A minor collection traces only the young generation, with
 * the old generation's marked cards as extra roots, and promotes the
 * survivors.  A full collection promotes everything first and then
 * collects the old generation, so it needs one marking pass too.
 *
 * Local collections on different threads run at the same time: all
 * the state of this one lives in the thread's ph_local_gc, and what it
 * shares with the rest of the world (the block table, page byte counts,
 * the partial page lists) is touched under the page table lock only. */
int process_heap_collect_impl(struct process_heap *h, void *approx_sp, int full)
{
    struct thread *th = get_sb_vm_thread();
    if (h->state != PROCESS_HEAP_LIVE) return -2;
    if (h->installed_on != th) return -1;
    if (gc_active_p) lose("process heap collection during a global collection");
    if (ph_local) lose("nested process heap collection");

    uword_t start_time = get_time();
    int running = atomic_fetch_add(&ph_local_gcs_running, 1) + 1;
    int peak = atomic_load(&ph_local_gcs_peak);
    while (running > peak
           && !atomic_compare_exchange_weak(&ph_local_gcs_peak, &peak, running));

    struct ph_local_gc *ctx = ph_thread_context(th);
    ctx->heap = h;
    ctx->owner = h->id;
    ctx->grey_list = NULL;
    ctx->blocks_in_flight = 0;
    ctx->wp_chain = WEAK_POINTER_CHAIN_END;
    ctx->wvectors = NULL;
    ctx->whash_tables = NULL;
    struct weak_pointer **saved_wp_chain = current_weak_pointer_chain;
    struct cons **saved_wvectors = current_weak_vectors;
    struct hash_table **saved_whash_tables = current_weak_hash_tables;
    struct hopscotch_table *saved_wobjects = current_weak_objects;
    current_weak_pointer_chain = &ctx->wp_chain;
    current_weak_vectors = &ctx->wvectors;
    current_weak_hash_tables = &ctx->whash_tables;
    current_weak_objects = &ctx->wobjects;
    extern bool compacting;
    compacting = 0;

    ph_close_installed_regions(th);
    process_heap_materialize(h);

    /* Policy: a full collection every FULLSWEEP_AFTER minors, or when the
     * old generation has doubled (by at least a megabyte) since the last
     * full collection or, before any, since the first promotion. */
    if (!full) {
        uword_t floor = h->gc_threshold > PH_OLD_GROWTH_FLOOR ? h->gc_threshold : PH_OLD_GROWTH_FLOOR;
        full = (h->fullsweep_after && h->minors_since_full >= h->fullsweep_after)
            || (h->bytes_old_at_full && h->bytes_old > 2 * h->bytes_old_at_full + floor);
    }

    acquire_gc_page_table_lock();
    if (full) ph_raise_generation(h, 0, 1);
    ctx->gen = full ? 1 : 0;
    ph_reset_large_pages(h, ctx->gen);
    release_gc_page_table_lock();

    ph_local = ctx;
    source_object = NULL;
    if (full) ph_outgoing_clear(h);

    /* Words above the stack pointer are dead frames whose contents may
     * refer to objects freed by an earlier collection; a frame pushed
     * later could expose them to the precise scan.  The global
     * collector scrubs for the same reason. */
    scrub_thread_control_stack(th);

    ph_scan_thread_roots(th, approx_sp);
    if (!full) ph_scan_old_cards(h);
    trace_everything();

    local_smash_weak_pointers();
    gc_dispose_private_pages();
    /* Culling conses the smashed cells of weak tables from the
     * collector's own region, which the page table lock protects. */
    acquire_gc_page_table_lock();
    cull_weak_hash_tables(mr_alivep_funs);
    release_gc_page_table_lock();

    acquire_gc_page_table_lock();
    ph_sweep(h, ctx->gen);
    /* Survivors of a minor collection become old. */
    if (!full) ph_raise_generation(h, 0, 1);
    h->bytes_old = ph_old_bytes(h);
    ph_clear_cards(h);
    release_gc_page_table_lock();

    ph_local = NULL;
    current_weak_pointer_chain = saved_wp_chain;
    current_weak_vectors = saved_wvectors;
    current_weak_hash_tables = saved_whash_tables;
    current_weak_objects = saved_wobjects;
    ctx->heap = NULL;
    if (full) {
        h->major_count++;
        h->minors_since_full = 0;
        h->bytes_old_at_full = h->bytes_old;
    } else {
        h->minor_count++;
        h->minors_since_full++;
        if (!h->bytes_old_at_full) h->bytes_old_at_full = h->bytes_old;
    }
    h->gc_count++;
    h->gc_pending = 0;
    h->gc_time_usec += get_time() - start_time;
    atomic_fetch_add(&ph_local_gcs_running, -1);
    if (process_heap_verify_after_gc) {
        /* Debugging aid; the violation log is shared, so concurrent
         * collections may attribute each other's findings. */
        process_heap_reset_violations();
        process_heap_verify(h);
        if (process_heap_violation_count())
            lose("process heap %u fails verification after collection %lu",
                 h->id, (unsigned long)h->gc_count);
    }
    return 0;
}

/* --- Verification --- */

static void ph_verify_slot(struct process_heap *h, lispobj *source,
                           lispobj *slot, lispobj value)
{
    if (!is_lisp_pointer(value)) return;
    page_index_t p = find_page_index((void*)value);
    if (p < 0) return;
    uint32_t owner = block_owner[address_block((void*)value)];
    if (page_free_p(p) || (owner != 0 && owner != h->id))
        process_heap_note_violation(source, slot, value);
}

static void ph_verify_object(struct process_heap *h, lispobj *where)
{
    if (!is_header(*where)) {
        ph_verify_slot(h, where, where, where[0]);
        ph_verify_slot(h, where, where + 1, where[1]);
        return;
    }
    lispobj header = *where;
    int widetag = header_widetag(header);
    if (instanceoid_widetag_p(widetag)) {
        lispobj layout = layout_of(where);
        if (!layout) return;
        int nslots = instanceoid_length(header);
        struct bitmap bitmap = get_layout_bitmap(LAYOUT(layout));
        lispobj *slots = where + 1;
        for (int i = 0; i < nslots; i++)
            if (bitmap_logbitp(i, bitmap))
                ph_verify_slot(h, where, slots + i, slots[i]);
        return;
    }
    if (leaf_obj_widetag_p(widetag) || widetag == FILLER_WIDETAG
        || widetag == CODE_HEADER_WIDETAG)
        return;
    sword_t n = object_size(where);
    for (sword_t i = 1; i < n; i++)
        ph_verify_slot(h, where, where + i, where[i]);
}

/* Check every pointer held by H's objects against the ownership
 * matrix.  H must be installed on the calling thread. */
int process_heap_verify(struct process_heap *h)
{
    struct thread *th = get_sb_vm_thread();
    if (h->state != PROCESS_HEAP_LIVE) return -2;
    if (h->installed_on != th) return -1;
    ph_close_installed_regions(th);
    process_heap_materialize(h);
    acquire_gc_page_table_lock();
    process_heap_check_blocks(h, "verify");
    release_gc_page_table_lock();
    for (sword_t j = 0; j < h->npages; j++) {
        page_index_t p = h->pages[j];
        if (page_single_obj_p(p)) {
            if (page_starts_contiguous_block_p(p))
                ph_verify_object(h, (lispobj*)page_address(p));
        } else {
            PH_FOR_RUNS(h, p, b, e)
                PH_FOR_OBJECTS_IN_LINES(where, block_to_line(b), block_to_line(e))
                    ph_verify_object(h, where);
        }
    }
    return 0;
}

#endif /* LISP_FEATURE_SB_PROCESS_HEAPS */
