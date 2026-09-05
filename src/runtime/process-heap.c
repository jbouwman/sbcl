/*
 * Per-process heaps: descriptors, ownership table, TLAB switching,
 * mailboxes, and the entry points called from Lisp.  Everything that
 * needs mark-region internals lives in process-heap-gc.inc, which is
 * included by mark-region.c.
 */

/*
 * This software is part of the SBCL system. See the README file for
 * more information.
 */

#include "genesis/sbcl.h"
#include "process-heap.h"

#ifdef LISP_FEATURE_SB_PROCESS_HEAPS

#include "gc.h"
#include "thread.h"
#include "interrupt.h"
#include "interr.h"
#include "pseudo-atomic.h"
#include "arch.h"
#include "genesis/symbol.h"
#include "genesis/static-symbols.h"
#include "fiber.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stddef.h>

extern int process_heap_debug;
int process_heap_verify_after_gc;
uint32_t *block_owner;
unsigned char *ph_process_page;
unsigned char *ph_page_partial;
struct process_heap **process_heap_table;
uint32_t process_heap_table_size;
uint32_t process_heap_count;
int process_heap_check_refs;

static pthread_mutex_t process_heap_table_lock = PTHREAD_MUTEX_INITIALIZER;
static uint32_t process_heap_next_epoch;

/* Caller holds the table lock. */
static struct process_heap *table_lookup(uint32_t id, uint32_t epoch)
{
    if (id == 0 || id > process_heap_table_size) return NULL;
    struct process_heap *h = process_heap_table[id - 1];
    return (h && h->epoch == epoch && h->state == PROCESS_HEAP_LIVE) ? h : NULL;
}

void process_heap_init(page_index_t npages)
{
    extern int process_heap_debug;
    block_owner = calloc((npages + 1) * PH_BLOCKS_PER_PAGE, sizeof (uint32_t));
    ph_process_page = calloc(npages + 1, 1);
    ph_page_partial = calloc(npages + 1, 1);
    gc_assert(block_owner && ph_process_page && ph_page_partial);
    process_heap_debug = getenv("SBCL_PH_DEBUG") != NULL;
    process_heap_verify_after_gc = getenv("SBCL_PH_VERIFY") != NULL;
}

uint32_t process_heap_owner_of(lispobj obj)
{
    if (!is_lisp_pointer(obj)) return 0;
    page_index_t p = find_page_index((void*)obj);
    if (p < 0) return 0;
    return page_free_p(p) ? 0 : block_owner[address_block((void*)obj)];
}

struct process_heap *process_heap_from_id(uint32_t id)
{
    if (id == 0 || id > process_heap_table_size) return NULL;
    return process_heap_table[id - 1];
}

/* --- Pseudo-atomic helpers (mirroring fiber.c) --- */

static inline void ph_enter_pa(struct thread *th)
{
#if defined LISP_FEATURE_ARM64
    ((volatile uint32_t *)&th->pseudo_atomic_bits)[0] = flag_PseudoAtomic;
#else
    th->pseudo_atomic_bits = (uword_t)th;
#endif
}

static inline void ph_exit_pa(struct thread *th)
{
#if defined LISP_FEATURE_ARM64
    volatile uint32_t *halves = (volatile uint32_t *)&th->pseudo_atomic_bits;
    halves[0] = 0;
    if (halves[1]) {
        halves[1] = 0;
        asm volatile("brk %0" : : "i"(trap_PendingInterrupt));
    }
#else
    uword_t pa = __sync_xor_and_fetch(&th->pseudo_atomic_bits, (uword_t)th);
    if (pa) {
#if defined LISP_FEATURE_UD2_BREAKPOINTS
        asm volatile("ud2\n\t.byte %c0" : : "i"(trap_PendingInterrupt));
#else
        asm volatile("ud2");
#endif
    }
#endif
}

/* --- Descriptor table --- */

/* Called with signals blocked: the GC iterates the table without a lock,
 * so the thread growing it must not be stoppable mid-update. */
static void table_register(struct process_heap *h)
{
    ignore_value(mutex_acquire(&process_heap_table_lock));
    uint32_t slot = 0;
    for (; slot < process_heap_table_size; slot++)
        if (!process_heap_table[slot]) break;
    if (slot == process_heap_table_size) {
        uint32_t newsize = process_heap_table_size ? 2 * process_heap_table_size : 64;
        struct process_heap **newtab = calloc(newsize, sizeof *newtab);
        gc_assert(newtab);
        if (process_heap_table)
            memcpy(newtab, process_heap_table,
                   process_heap_table_size * sizeof *newtab);
        struct process_heap **old = process_heap_table;
        process_heap_table = newtab;
        __atomic_store_n(&process_heap_table_size, newsize, __ATOMIC_RELEASE);
        free(old);
    }
    h->id = slot + 1;
    h->epoch = ++process_heap_next_epoch;
    process_heap_table[slot] = h;
    process_heap_count++;
    ignore_value(mutex_release(&process_heap_table_lock));
}

/* Idempotent: process_heap_release_id takes a heap out of the table
 * before anything else. */
static void table_unregister(struct process_heap *h)
{
    ignore_value(mutex_acquire(&process_heap_table_lock));
    gc_assert(h->id);
    if (process_heap_table[h->id - 1] == h) {
        process_heap_table[h->id - 1] = NULL;
        process_heap_count--;
    }
    ignore_value(mutex_release(&process_heap_table_lock));
}

void process_heap_add_page(struct process_heap *h, page_index_t p)
{
    if (h->npages == h->pages_capacity) {
        sword_t cap = h->pages_capacity ? 2 * h->pages_capacity : 16;
        page_index_t *np = realloc(h->pages, cap * sizeof *np);
        gc_assert(np);
        h->pages = np;
        h->pages_capacity = cap;
    }
    h->pages[h->npages++] = p;
}

void process_heap_set_flags(struct process_heap *h, int flags)
{
    h->store_check = (flags & PROCESS_HEAP_FLAG_SIGNAL_STORES) ? PROCESS_HEAP_STORES_SIGNALED
        : (flags & PROCESS_HEAP_FLAG_RECORD_STORES) ? PROCESS_HEAP_STORES_RECORDED
        : PROCESS_HEAP_STORES_UNCHECKED;
    h->strict = (flags & PROCESS_HEAP_FLAG_STRICT) != 0;
    if (h->installed_on)
        h->installed_on->process_heap_check = h->store_check ? h : NULL;
}

struct process_heap *process_heap_create(int kind, uword_t gc_threshold,
                                         uword_t hard_limit, int flags)
{
    struct process_heap *h = calloc(1, sizeof *h);
    if (!h) return NULL;
    gc_set_region_empty(&h->parked_cons);
    gc_set_region_empty(&h->parked_mixed);
    h->kind = kind;
    h->state = PROCESS_HEAP_LIVE;
    h->gc_threshold = gc_threshold;
    h->hard_limit = hard_limit;
    h->fullsweep_after = 16;
    process_heap_set_flags(h, flags);
    pthread_mutex_init(&h->mailbox_lock, 0);
    sigset_t old;
    block_blockable_signals(&old);
    table_register(h);
    thread_sigmask(SIG_SETMASK, &old, 0);
    return h;
}

static void free_descriptor(struct process_heap *h)
{
    pthread_mutex_destroy(&h->mailbox_lock);
    free(h->pages);
    free(h->outgoing);
    free(h);
}

/* Release a heap: return its pages (and those of any queued fragments)
 * to the free pool and drop the descriptor.  Must not be called while a
 * local GC of the heap is in progress; that can only happen on the
 * thread the heap is installed on, so a caller on that thread is safe,
 * and 'force' is for reaping the heaps of a thread that has exited. */
int process_heap_release_internal(struct process_heap *h, int force)
{
    struct thread *th = get_sb_vm_thread();
    if (h->state != PROCESS_HEAP_LIVE && h->state != PROCESS_HEAP_RELEASING) return -2;
    if (h->installed_on) {
        if (h->installed_on == th) process_heap_switch(th, NULL);
        else if (!force) return -1;
        else h->installed_on = NULL;
    }
    sigset_t old;
    block_blockable_signals(&old);

    /* Drain the mailbox first so no sender can add to it afterwards. */
    ignore_value(mutex_acquire(&h->mailbox_lock));
    h->state = PROCESS_HEAP_RELEASED;
    struct process_heap *frag = h->mailbox_head;
    h->mailbox_head = h->mailbox_tail = NULL;
    h->mailbox_count = 0;
    h->mailbox_bytes = 0;
    ignore_value(mutex_release(&h->mailbox_lock));

    acquire_gc_page_table_lock();
    while (frag) {
        struct process_heap *next = frag->next_in_mailbox;
        process_heap_free_pages_locked(frag);
        frag->state = PROCESS_HEAP_RELEASED;
        table_unregister(frag);
        free_descriptor(frag);
        frag = next;
    }
    process_heap_free_pages_locked(h);
    release_gc_page_table_lock();
    table_unregister(h);
    thread_sigmask(SIG_SETMASK, &old, 0);
    free_descriptor(h);
    return 0;
}

int process_heap_release(struct process_heap *h)
{
    return process_heap_release_internal(h, 0);
}

/* Release by handle: the heap becomes RELEASING under the lock first,
 * so no other thread can find it afterwards, and a thread that found
 * it earlier has finished with it, since it held the lock while it did.
 * Its slot stays taken until process_heap_release_internal has untagged
 * its blocks: a new heap with its id would otherwise take the old
 * blocks for its own. */
int process_heap_release_id(uint32_t id, uint32_t epoch)
{
    struct thread *th = get_sb_vm_thread();
    ignore_value(mutex_acquire(&process_heap_table_lock));
    struct process_heap *h = table_lookup(id, epoch);
    if (!h || h->state != PROCESS_HEAP_LIVE) {
        ignore_value(mutex_release(&process_heap_table_lock));
        return -2;
    }
    if (h->installed_on && h->installed_on != th) {
        ignore_value(mutex_release(&process_heap_table_lock));
        return -1;
    }
    h->state = PROCESS_HEAP_RELEASING;
    ignore_value(mutex_release(&process_heap_table_lock));
    return process_heap_release_internal(h, 0);
}

/* --- TLAB switching --- */

struct process_heap *process_heap_current(struct thread *th)
{
    return thread_extra_data(th)->current_heap;
}

int process_heap_switch_in_pa(struct thread *th, struct process_heap *to)
{
    struct extra_thread_data *ed = thread_extra_data(th);
    struct process_heap *from = ed->current_heap;
    if (from == to) return 0;
    if (to && to->state != PROCESS_HEAP_LIVE) return -2;
    if (to && to->installed_on && to->installed_on != th) return -1;
    if (from) {
        from->parked_mixed = th->mixed_tlab;
        from->parked_cons  = th->cons_tlab;
        from->installed_on = NULL;
    } else {
        ed->saved_mixed_tlab = th->mixed_tlab;
        ed->saved_cons_tlab  = th->cons_tlab;
    }
    if (to) {
        th->mixed_tlab = to->parked_mixed;
        th->cons_tlab  = to->parked_cons;
        /* The thread's TLABs are the only copy while TO is installed. */
        gc_set_region_empty(&to->parked_mixed);
        gc_set_region_empty(&to->parked_cons);
        to->installed_on = th;
    } else {
        th->mixed_tlab = ed->saved_mixed_tlab;
        th->cons_tlab  = ed->saved_cons_tlab;
    }
    ed->current_heap = to;
    th->process_heap_check = (to && to->store_check) ? to : NULL;
#ifdef LISP_FEATURE_SB_FIBER
    if (ed->current_fiber) ed->current_fiber->active_heap = to;
#endif
    return 0;
}

int process_heap_switch(struct thread *th, struct process_heap *to)
{
    ph_enter_pa(th);
    int rc = process_heap_switch_in_pa(th, to);
    ph_exit_pa(th);
    return rc;
}

/* Entry points for Lisp that take and return raw addresses, so that
 * switching never needs to allocate a boxed SAP: the switch away from
 * an exhausted heap must not allocate into it. */
int process_heap_switch_address(uword_t heap)
{
    return process_heap_switch(get_sb_vm_thread(), (struct process_heap*)heap);
}

uword_t process_heap_current_address(void)
{
    return (uword_t)thread_extra_data(get_sb_vm_thread())->current_heap;
}

/* --- Local collection entry points --- */

int process_heap_collect(struct process_heap *h, int full)
{
    void *sp = __builtin_frame_address(0);
    return process_heap_collect_impl(h, sp, full);
}

void process_heap_set_fullsweep_after(struct process_heap *h, uword_t n)
{
    h->fullsweep_after = n;
}

bool process_heap_gc_pending_p(struct thread *th)
{
    struct process_heap *h = thread_extra_data(th)->current_heap;
    return h && h->gc_pending;
}

int process_heap_collect_pending(void)
{
    struct thread *th = get_sb_vm_thread();
    struct process_heap *h = thread_extra_data(th)->current_heap;
    if (!h) return 0;
    h->gc_pending = 0;
    return process_heap_collect(h, 0);
}

/* Called from interrupt_handle_pending with blockable signals blocked
 * and GC not inhibited.  Runs the Lisp side (which wraps the collection
 * in WITHOUT-GCING) on top of the interrupted context, exactly as
 * maybe_gc does for a global collection. */
void process_heap_run_pending_gc(os_context_t *context)
{
    struct thread *th = get_sb_vm_thread();
    bool were_in_lisp = !foreign_function_call_active_p(th);
    if (were_in_lisp) fake_foreign_function_call(context);
    funcall0(StaticSymbolFunction(PROCESS_HEAP_COLLECT_PENDING));
    if (were_in_lisp) undo_fake_foreign_function_call(context);
    else block_blockable_signals(0);
}

void process_heap_exhausted(struct process_heap *h, sword_t nbytes)
{
    struct thread *th = get_sb_vm_thread();
    uword_t available = h->hard_limit > h->bytes_allocated
        ? h->hard_limit - h->bytes_allocated : 0;
    available &= ~(uword_t)LOWTAG_MASK;
    gc_assert(get_pseudo_atomic_atomic(th));
    /* A local collection request is found through current_heap. H is about
     * to be switched out, so retract its request before dispatching other
     * pending work. */
    bool retract_collection = h->gc_pending;
    h->gc_pending = 0;
    /* Nothing more can be allocated in H, so the error must be built and
     * signaled with the global heap installed.  Lisp reinstalls H (see
     * process_heap_take_exhausted) if the error is handled inside the
     * heap's dynamic extent. */
    process_heap_switch_in_pa(th, NULL);
    thread_extra_data(th)->exhausted_heap = h;
    clear_pseudo_atomic_atomic(th);
    if (retract_collection
        && read_TLS(GC_PENDING, th) == NIL
#if THREADS_USING_GCSIGNAL
        && read_TLS(STOP_FOR_GC_PENDING, th) == NIL
#endif
        && !thread_interrupt_data(th).pending_handler)
        arch_clear_pseudo_atomic_interrupted(th);
    /* The collection request may also have blocked deferrable signals.
     * Run the pending handler after retracting it so that state is restored. */
    if (retract_collection || get_pseudo_atomic_interrupted(th))
        do_pending_interrupt();
    /* Both values are double-word aligned, so they read as fixnums. */
    funcall2(StaticSymbolFunction(PROCESS_HEAP_EXHAUSTED_ERROR),
             available, nbytes);
    lose("PROCESS-HEAP-EXHAUSTED-ERROR fell through");
}

/* The store barrier's slow path: classify a pointer store of VALUE into
 * OBJECT against the ownership matrix.  Non-pointer values and stores
 * into stack or static objects are never violations; a store into a
 * global object is one only in strict mode. */
void process_heap_check_store(lispobj value, lispobj object)
{
    struct thread *th = get_sb_vm_thread();
    struct process_heap *h = th->process_heap_check;
    if (!h) return;
    /* Stack-allocated objects belong to the storing process. */
    if (!is_lisp_pointer(object) || is_in_stack_space(object)) return;
    uint32_t value_owner = process_heap_owner_of(value);
    uint32_t object_owner = process_heap_owner_of(object);
    int kind = 0;
    if (value_owner && value_owner != object_owner)
        kind = object_owner ? PROCESS_HEAP_STORE_CROSS_HEAP : PROCESS_HEAP_STORE_ESCAPE;
    else if (h->strict && !object_owner)
        kind = PROCESS_HEAP_STORE_GLOBAL;
    if (!kind) return;
    process_heap_note_violation(is_lisp_pointer(object) ? native_pointer(object) : NULL,
                                NULL, value);
    if (process_heap_debug)
        fprintf(stderr, "ph store violation kind %d: object %p value %p\n",
                kind, (void*)object, (void*)value);
    if (h->store_check != PROCESS_HEAP_STORES_SIGNALED) return;
    /* The assembly routine enters pseudo-atomic before calling us. */
    if (get_pseudo_atomic_atomic(th)) {
        clear_pseudo_atomic_atomic(th);
        if (get_pseudo_atomic_interrupted(th))
            do_pending_interrupt();
    }
    funcall3(StaticSymbolFunction(PROCESS_HEAP_STORE_ERROR),
             object, value, make_fixnum(kind));
}

uword_t process_heap_take_exhausted(void)
{
    struct thread *th = get_sb_vm_thread();
    struct process_heap *h = thread_extra_data(th)->exhausted_heap;
    thread_extra_data(th)->exhausted_heap = NULL;
    return (uword_t)h;
}

/* --- Violation log (filled during tracing, possibly from GC threads) --- */

#define MAX_VIOLATIONS 64
static lispobj violations[MAX_VIOLATIONS][3];
static int nviolations;

void process_heap_note_violation(lispobj *source, lispobj *slot, lispobj target)
{
    int i = __atomic_fetch_add(&nviolations, 1, __ATOMIC_ACQ_REL);
    if (i < MAX_VIOLATIONS) {
        violations[i][0] = source ? compute_lispobj(source) : 0;
        violations[i][1] = (lispobj)slot;
        violations[i][2] = target;
    }
}

int process_heap_violation_count(void)
{
    int n = nviolations;
    return n > MAX_VIOLATIONS ? MAX_VIOLATIONS : n;
}

int process_heap_get_violation(int i, lispobj *out)
{
    if (i < 0 || i >= process_heap_violation_count()) return 0;
    out[0] = violations[i][0];
    out[1] = violations[i][1];
    out[2] = violations[i][2];
    return 1;
}

void process_heap_reset_violations(void)
{
    nviolations = 0;
}

/* --- Mailbox --- */

int process_heap_seal(struct process_heap *f)
{
    if (f->kind != PROCESS_HEAP_FRAGMENT || f->state != PROCESS_HEAP_LIVE)
        return -3;
    if (f->installed_on) return -1;
    /* Closing touches page metadata that a concurrent global GC would
     * also close; keep the two from interleaving. */
    sigset_t old;
    block_blockable_signals(&old);
    ensure_region_closed(&f->parked_mixed, PAGE_TYPE_MIXED);
    ensure_region_closed(&f->parked_cons, PAGE_TYPE_CONS);
    thread_sigmask(SIG_SETMASK, &old, 0);
    return 0;
}

int process_heap_send_id(uint32_t dest_id, uint32_t dest_epoch,
                         struct process_heap *f, uint32_t sender_id)
{
    ignore_value(mutex_acquire(&process_heap_table_lock));
    struct process_heap *dest = table_lookup(dest_id, dest_epoch);
    int rc = dest ? process_heap_send(dest, f, sender_id) : -2;
    ignore_value(mutex_release(&process_heap_table_lock));
    return rc;
}

uword_t process_heap_stat_id(uint32_t id, uint32_t epoch, int which)
{
    ignore_value(mutex_acquire(&process_heap_table_lock));
    struct process_heap *h = table_lookup(id, epoch);
    uword_t value = h ? process_heap_stat(h, which)
        : which == 10 ? PROCESS_HEAP_RELEASED : 0;
    ignore_value(mutex_release(&process_heap_table_lock));
    return value;
}

int process_heap_send(struct process_heap *dest, struct process_heap *f,
                      uint32_t sender_id)
{
    if (f->kind != PROCESS_HEAP_FRAGMENT || f->state != PROCESS_HEAP_LIVE
        || f->installed_on)
        return -3;
    if (dest->kind != PROCESS_HEAP_PROCESS) return -3;
    ignore_value(mutex_acquire(&dest->mailbox_lock));
    if (dest->state != PROCESS_HEAP_LIVE) {
        ignore_value(mutex_release(&dest->mailbox_lock));
        return -2;
    }
    f->sender_id = sender_id;
    f->next_in_mailbox = NULL;
    if (dest->mailbox_tail) dest->mailbox_tail->next_in_mailbox = f;
    else dest->mailbox_head = f;
    dest->mailbox_tail = f;
    dest->mailbox_count++;
    dest->mailbox_bytes += f->bytes_allocated;
    ignore_value(mutex_release(&dest->mailbox_lock));
    return 0;
}

/* Retag a fragment's blocks as belonging to DEST.  No payload copy. */
void process_heap_adopt(struct process_heap *dest, struct process_heap *f)
{
    sigset_t old;
    block_blockable_signals(&old);
    acquire_gc_page_table_lock();
    for (sword_t i = 0; i < f->npages; i++) {
        page_index_t p = f->pages[i];
        bool listed = false;
        for (block_index_t b = page_to_block(p), e = b + PH_BLOCKS_PER_PAGE; b < e; b++) {
            if (block_owner[b] == dest->id) listed = true;
            else if (block_owner[b] == f->id) {
                block_owner[b] = dest->id;
                dest->nblocks++;
                f->nblocks--;
            }
        }
        if (!listed) process_heap_add_page(dest, p);
    }
    dest->bytes_allocated += f->bytes_allocated;
    dest->bytes_since_gc  += f->bytes_allocated;
    f->npages = 0;
    f->bytes_allocated = 0;
    f->state = PROCESS_HEAP_RELEASED;
    release_gc_page_table_lock();
    table_unregister(f);
    thread_sigmask(SIG_SETMASK, &old, 0);
    free_descriptor(f);
}

lispobj process_heap_receive(struct process_heap *h, int *found,
                             uint32_t *sender_id)
{
    struct thread *th = get_sb_vm_thread();
    *found = 0;
    *sender_id = 0;
    if (h->installed_on != th) { *found = -1; return 0; }
    ignore_value(mutex_acquire(&h->mailbox_lock));
    struct process_heap *f = h->mailbox_head;
    if (!f) {
        ignore_value(mutex_release(&h->mailbox_lock));
        return 0;
    }
    h->mailbox_head = f->next_in_mailbox;
    if (!h->mailbox_head) h->mailbox_tail = NULL;
    h->mailbox_count--;
    h->mailbox_bytes -= f->bytes_allocated;
    ignore_value(mutex_release(&h->mailbox_lock));
    lispobj root = f->root;
    *sender_id = f->sender_id;
    process_heap_adopt(h, f);
    *found = 1;
    return root;
}

int process_heap_root_offset(void)
{
    return offsetof(struct process_heap, root);
}

uword_t process_heap_stat(struct process_heap *h, int which)
{
    switch (which) {
    case 0: return h->bytes_allocated;
    case 1: return h->bytes_live;
    case 2: return h->bytes_since_gc;
    case 3: return h->gc_count;
    case 4: return h->npages;
    case 5: return h->mailbox_count;
    case 6: return h->mailbox_bytes;
    case 7: return h->gc_time_usec;
    case 8: return h->gc_threshold;
    case 9: return h->hard_limit;
    case 10: return h->state;
    case 11: return h->id;
    case 12: return (uword_t)h->installed_on;
    case 13: return h->store_check;
    case 14: return h->strict;
    case 15: return h->minor_count;
    case 16: return h->major_count;
    case 17: return h->bytes_old;
    case 18: return h->fullsweep_after;
    case 19: return h->nblocks;
    case 20: return h->noutgoing;
    case 21: return h->global_root_objects;
    case 22: return process_heap_concurrency_peak();
    case 23: return h->epoch;
    default: return 0;
    }
}

#endif /* LISP_FEATURE_SB_PROCESS_HEAPS */
