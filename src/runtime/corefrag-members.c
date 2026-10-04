/*
 * Core fragments built in a build heap.  A module is loaded with a
 * build heap installed and a fragment recorder bound; the recorder's
 * records name everything global objects must come to refer to when the
 * fragment is activated.  The fragment's objects are then the heap's
 * objects reachable from the recorder through objects of the heap: what
 * the load left in the heap and nothing reaches (loader state, the code
 * of top-level forms, discarded closures) is left out.
 *
 * The trace follows the collector's own description of every object's
 * pointers (trace-object.inc), with weak references taken as strong.
 */

/*
 * This software is part of the SBCL system. See the README file for
 * more information.
 */

#include "genesis/sbcl.h"
#include "local-heap.h"

#ifdef LISP_FEATURE_SB_LOCAL_HEAPS

#include <stdlib.h>
#include "gc.h"
#include "code.h"
#include "interr.h"
#include "thread.h"
#include "var-io.h"
#include "genesis/cons.h"
#include "genesis/gc-tables.h"
#include "genesis/instance.h"
#include "genesis/closure.h"
#include "genesis/hash-table.h"
#include "genesis/symbol.h"
#include "genesis/split-ordered-list.h"

/* One trace at a time per thread; ACTION has no argument to carry it. */
struct corefrag_trace {
    struct local_heap *heap;
    int32_t *page_slot;     /* page index -> the page's slot in VISITED, or -1 */
    uword_t *visited;       /* one bit per two-word granule of each heap page */
    lispobj *members;       /* in the order found, which is also the work queue */
    uword_t nmembers, capacity;
};
static _Thread_local struct corefrag_trace *corefrag;

#define GRANULES_PER_PAGE (GENCGC_PAGE_BYTES / (2 * N_WORD_BYTES))

static void corefrag_visit(lispobj x)
{
    struct corefrag_trace *t = corefrag;
    if (!is_lisp_pointer(x) || find_page_index((void*)x) < 0) return;
    lispobj *base = native_pointer(x);
    if (block_owner[address_block(base)] != t->heap->id) return;
    /* A simple-fun is part of its code object. */
    if (lowtag_of(x) == FUN_POINTER_LOWTAG && embedded_obj_p(widetag_of(base))) {
        base = (lispobj*)fun_code_header((struct simple_fun*)base);
        x = make_lispobj(base, OTHER_POINTER_LOWTAG);
    }
    page_index_t p = find_page_index(base);
    int32_t slot = t->page_slot[p];
    if (slot < 0) lose("corefrag: %p is owned by heap %u, which lists no page %ld",
                       base, t->heap->id, (long)p);
    uword_t bit = (uword_t)slot * GRANULES_PER_PAGE
        + ((char*)base - (char*)page_address(p)) / (2 * N_WORD_BYTES);
    uword_t mask = (uword_t)1 << (bit % N_WORD_BITS);
    if (t->visited[bit / N_WORD_BITS] & mask) return;
    t->visited[bit / N_WORD_BITS] |= mask;
    if (t->nmembers == t->capacity) {
        t->capacity = t->capacity ? 2 * t->capacity : 4096;
        t->members = realloc(t->members, t->capacity * sizeof (lispobj));
        if (!t->members) lose("corefrag: out of memory");
    }
    t->members[t->nmembers++] = x;
}

/* Everything trace-object.inc needs for weak objects, which it takes as
 * strong here, is reached only from branches that are never taken. */
static inline bool pointer_survived_gc_yet(__attribute__((unused)) lispobj obj) { return 1; }
#define interesting_pointer_p(x) 0
#define TRACE_NAME corefrag_trace_object
#define ACTION(x, where, source) corefrag_visit(x)
#define STRENGTHEN_WEAK_REFS 1
#define HT_ENTRY_LIVENESS_FUN_ARRAY_NAME corefrag_alivep_funs
#include "trace-object.inc"

static int compare_lispobj(const void *a, const void *b)
{
    lispobj x = *(const lispobj*)a, y = *(const lispobj*)b;
    return x < y ? -1 : x > y;
}

/* Trace the objects of the sealed build heap H reachable from ROOT, an
 * object of H or a vector of roots outside it, through objects of H, and
 * keep them in H, sorted by address, for
 * local_heap_fragment_member.  Return their number, or -3 if H is not a
 * sealed build heap.  Global collection must be inhibited. */
sword_t local_heap_fragment_trace(struct local_heap *h, lispobj root)
{
    if (h->kind != LOCAL_HEAP_BUILD || !h->sealed) return -3;
    struct corefrag_trace t = { .heap = h };
    t.page_slot = malloc(page_table_pages * sizeof (int32_t));
    t.visited = calloc(h->npages ? h->npages : 1,
                       GRANULES_PER_PAGE / N_WORD_BITS * sizeof (uword_t));
    if (!t.page_slot || !t.visited) lose("corefrag: out of memory");
    for (page_index_t p = 0; p < page_table_pages; p++) t.page_slot[p] = -1;
    for (sword_t i = 0; i < h->npages; i++) t.page_slot[h->pages[i]] = i;

    corefrag = &t;
    /* ROOT is an object of H, or a vector outside H whose elements are
     * the roots. */
    if (is_lisp_pointer(root) && find_page_index((void*)root) >= 0
        && block_owner[address_block(native_pointer(root))] == h->id)
        corefrag_visit(root);
    else
        corefrag_trace_object(native_pointer(root));
    for (uword_t i = 0; i < t.nmembers; i++) {
        lispobj object = t.members[i];
        if (listp(object)) {
            struct cons *c = CONS(object);
            corefrag_visit(c->car);
            corefrag_visit(c->cdr);
        } else {
            lispobj *where = native_pointer(object);
            /* trace-object.inc leaves out the layout of an instance
             * whose header holds it. */
            if (instanceoid_widetag_p(widetag_of(where)))
                corefrag_visit(layout_of(where));
            corefrag_trace_object(where);
        }
    }
    corefrag = NULL;

    free(t.page_slot);
    free(t.visited);
    qsort(t.members, t.nmembers, sizeof (lispobj), compare_lispobj);
    free(h->members);
    h->members = t.members;
    h->nmembers = t.nmembers;
    return t.nmembers;
}

/* The Ith object of H's fragment, by address. */
lispobj local_heap_fragment_member(struct local_heap *h, uword_t i)
{
    return i < h->nmembers ? h->members[i] : 0;
}

#endif /* LISP_FEATURE_SB_LOCAL_HEAPS */
