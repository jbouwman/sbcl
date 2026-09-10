/*
 * Per-process heaps: GC ownership domains carved out of dynamic space.
 *
 * A process heap is a set of GC pages that belong to one "process"
 * (typically an sb-fiber coroutine).  Objects in a process heap may
 * refer to objects in the same heap and to global objects; no global
 * object and no other heap may refer into it.  This invariant lets a
 * heap be collected by itself (see process_heap_collect) and released
 * wholesale when its process exits.
 *
 * Ownership is recorded per 4 KiB block in 'block_owner' (0 = free or
 * global); a page holding any process block is flagged in
 * 'ph_process_page' and is never used by the global allocator.
 */

/*
 * This software is part of the SBCL system. See the README file for
 * more information.
 */

#ifndef SBCL_PROCESS_HEAP_H
#define SBCL_PROCESS_HEAP_H

#include "lispobj.h"

#ifdef LISP_FEATURE_SB_PROCESS_HEAPS

#include "gencgc-alloc-region.h"
#include "gc-typedefs.h"
#include "os.h"
#include "globals.h"
#include <stdint.h>
#include <stdbool.h>
#include <pthread.h>

struct thread;

enum process_heap_kind {
    PROCESS_HEAP_PROCESS  = 1,  /* attached to a process; collected locally */
    PROCESS_HEAP_FRAGMENT = 2   /* a sealed message; adopted by the receiver */
};

enum process_heap_state {
    PROCESS_HEAP_LIVE      = 1,
    PROCESS_HEAP_RELEASED  = 2,
    /* Being released by id: no longer found by lookups, but its slot
     * stays taken until its blocks are untagged, so that a new heap
     * cannot be given its id while the block table still names it. */
    PROCESS_HEAP_RELEASING = 3
};

/* What the store barrier does with an ownership violation. */
enum process_heap_store_check {
    PROCESS_HEAP_STORES_UNCHECKED = 0,
    PROCESS_HEAP_STORES_RECORDED  = 1,
    PROCESS_HEAP_STORES_SIGNALED  = 2
};

/* Kinds of store violation, passed to Lisp. */
enum process_heap_store_kind {
    PROCESS_HEAP_STORE_ESCAPE     = 1,  /* global object <- process object */
    PROCESS_HEAP_STORE_CROSS_HEAP = 2,  /* process object <- other heap's object */
    PROCESS_HEAP_STORE_GLOBAL     = 3   /* strict: global object mutated at all */
};

/* Flags for process_heap_create. */
#define PROCESS_HEAP_FLAG_RECORD_STORES 1
#define PROCESS_HEAP_FLAG_SIGNAL_STORES 2
#define PROCESS_HEAP_FLAG_STRICT        4

/* Ownership granularity.  Blocks are aligned inside pages, and a
 * region never crosses from one heap's blocks into another's. */
#define PH_BLOCK_BYTES 4096
#define PH_BLOCK_LINES (PH_BLOCK_BYTES / (8 << N_LOWTAG_BITS))
#define PH_BLOCKS_PER_PAGE ((int)(GENCGC_PAGE_BYTES / PH_BLOCK_BYTES))
typedef sword_t block_index_t;

struct process_heap {
    uint32_t id;              /* index+1 into process_heap_table; block_owner value */
    uint32_t kind;            /* enum process_heap_kind */
    uint32_t state;           /* enum process_heap_state */
    uint32_t gc_pending;      /* a local GC was requested at an allocation slow path */
    uint32_t epoch;           /* bumped when the descriptor is reused */
    uint32_t store_check;     /* enum process_heap_store_check */
    uint32_t strict;          /* stores into global objects are violations too */

    /* Parked allocation regions while the heap is not installed on a thread. */
    struct alloc_region parked_cons;
    struct alloc_region parked_mixed;

    /* Every page on which this heap owns a block, small and large alike. */
    page_index_t *pages;
    sword_t npages;
    sword_t pages_capacity;
    sword_t reuse_cursor;     /* first page that might still have free lines */
    uword_t nblocks;          /* blocks owned, large pages included */

    uword_t bytes_allocated;  /* block-granular bytes claimed from dynamic space */
    uword_t bytes_live;       /* bytes retained by the most recent local GC */
    uword_t bytes_since_gc;   /* bytes claimed since the most recent local GC */
    uword_t gc_threshold;     /* auto-collect when bytes_since_gc exceeds
                               * max(gc_threshold, bytes_live); 0 disables */
    uword_t hard_limit;       /* refuse to grow past this many bytes; 0 = none */
    uword_t gc_count;
    uword_t gc_time_usec;

    /* Generations: allocation is young (line/page generation 0), a minor
     * collection promotes survivors to generation 1. */
    uword_t minor_count;
    uword_t major_count;
    uword_t minors_since_full;
    uword_t fullsweep_after;  /* full collection after this many minors; 0 = never automatic */
    uword_t bytes_old;        /* bytes in generation 1 after the last collection */
    uword_t bytes_old_at_full;/* bytes_old right after the last full collection */

    /* Outgoing global-root summary: the global objects the heap's old
     * generation referred to when it was last traced, as an open
     * addressing hash set (0 = empty slot).  A global collection marks
     * these instead of walking the old generation; see
     * process_heaps_trace_roots. */
    lispobj *outgoing;
    uword_t noutgoing;
    uword_t outgoing_mask;       /* table size - 1, or 0 when there is no table */
    uword_t global_root_objects; /* objects walked by the last global collection */

    /* Thread whose user TLABs currently belong to this heap, or NULL. */
    struct thread *installed_on;

    /* Mailbox of sealed fragments (kind PROCESS only). */
    pthread_mutex_t mailbox_lock;
    struct process_heap *mailbox_head;
    struct process_heap *mailbox_tail;
    sword_t mailbox_count;
    uword_t mailbox_bytes;

    /* Fragment fields (kind FRAGMENT only). */
    lispobj root;
    struct process_heap *next_in_mailbox;
    uint32_t sender_id;
};

/* Ownership metadata: one entry per block (0 = free or global), and per
 * page whether any of its blocks belong to a process heap, and whether
 * the page is listed as having free blocks. */
extern uint32_t *block_owner;
extern unsigned char *ph_process_page;
extern unsigned char *ph_page_partial;

static inline block_index_t address_block(void *address)
{
    return ((uintptr_t)address - DYNAMIC_SPACE_START) / PH_BLOCK_BYTES;
}
static inline block_index_t page_to_block(page_index_t p)
{
    return (block_index_t)p * PH_BLOCKS_PER_PAGE;
}

/* Heap descriptor table. Slot i holds the heap with id i+1. */
extern struct process_heap **process_heap_table;
extern uint32_t process_heap_table_size;
extern uint32_t process_heap_count;

/* When nonzero, tracing records ownership violations. */
extern int process_heap_check_refs;
/* SBCL_PH_VERIFY: verify a heap after each of its collections. */
extern int process_heap_verify_after_gc;

void process_heap_init(page_index_t npages);
uint32_t process_heap_owner_of(lispobj obj);
struct process_heap *process_heap_from_id(uint32_t id);

/* Handles for threads other than the owner: a heap id may be reused
 * once a heap is released, so an (id, epoch) pair names a heap for as
 * long as it lives, and these entry points look it up under the table
 * lock, which a release also needs. */
int  process_heap_send_id(uint32_t dest_id, uint32_t dest_epoch,
                          struct process_heap *fragment, uint32_t sender_id);
uword_t process_heap_stat_id(uint32_t id, uint32_t epoch, int which);
int  process_heap_release_id(uint32_t id, uint32_t epoch);

/* Lifecycle */
struct process_heap *process_heap_create(int kind, uword_t gc_threshold,
                                         uword_t hard_limit, int flags);
void process_heap_set_flags(struct process_heap *h, int flags);
int  process_heap_release(struct process_heap *h);
int  process_heap_release_internal(struct process_heap *h, int force);

/* TLAB switching */
struct process_heap *process_heap_current(struct thread *th);
int  process_heap_switch(struct thread *th, struct process_heap *h);
int  process_heap_switch_in_pa(struct thread *th, struct process_heap *h);
void process_heap_add_page(struct process_heap *h, page_index_t p);

/* Allocation slow path (mark-region.c). Called inside pseudo-atomic. */
lispobj *process_heap_alloc_slow(struct thread *th, struct process_heap *h,
                                 struct alloc_region *region,
                                 sword_t nbytes, int page_type);
void process_heap_exhausted(struct process_heap *h, sword_t nbytes)
    __attribute__((noreturn));
uword_t process_heap_take_exhausted(void);

/* Store barrier slow path, called from the process-heap-store-check
 * assembly routine with the value being stored and the object stored
 * into. */
void process_heap_check_store(lispobj value, lispobj object);
int  process_heap_switch_address(uword_t heap);
uword_t process_heap_current_address(void);

/* Local collection.  The heap must be installed on the calling thread
 * and the caller must inhibit global GC (WITHOUT-GCING). */
int  process_heap_collect(struct process_heap *h, int full);
int  process_heap_collect_impl(struct process_heap *h, void *approx_sp, int full);
void process_heap_set_fullsweep_after(struct process_heap *h, uword_t n);
int  process_heap_collect_pending(void);
bool process_heap_gc_pending_p(struct thread *th);
void process_heap_run_pending_gc(os_context_t *context);
void process_heap_thread_exit(struct thread *th);
uword_t process_heap_concurrency_peak(void);

/* Global GC integration (mark-region.c). */
void process_heaps_before_global_gc(void);
void process_heaps_trace_roots(void);
void process_heap_free_pages_locked(struct process_heap *h);
void process_heap_materialize(struct process_heap *h);

/* Ownership verification. */
int  process_heap_verify(struct process_heap *h);
void process_heap_note_violation(lispobj *source, lispobj *slot, lispobj target);
int  process_heap_violation_count(void);
int  process_heap_violation_capacity(void);
int  process_heap_get_violation(int i, lispobj *out);
int  process_heap_take_violations(lispobj *out, int capacity, int *ndetails);
void process_heap_reset_violations(void);

/* Mailbox: fragments are process heaps of kind FRAGMENT. */
int  process_heap_seal(struct process_heap *fragment);
int  process_heap_send(struct process_heap *dest, struct process_heap *fragment,
                       uint32_t sender_id);
lispobj process_heap_receive(struct process_heap *h, int *found,
                             uint32_t *sender_id);
void process_heap_adopt(struct process_heap *dest, struct process_heap *fragment);
void process_heap_check_blocks(struct process_heap *h, const char *where);

/* Statistics */
uword_t process_heap_stat(struct process_heap *h, int which);

#endif /* LISP_FEATURE_SB_PROCESS_HEAPS */
#endif /* SBCL_PROCESS_HEAP_H */
