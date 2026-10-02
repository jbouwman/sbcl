#include "fiber.h"
#include "align.h"
#include "genesis/symbol.h"
#include "genesis/static-symbols.h"
#include "os.h"
#include "thread.h"
#include "validate.h"
#ifdef LISP_FEATURE_SB_LOCAL_HEAPS
#include "local-heap.h"
#endif
#include <stdint.h>
#include <string.h>
#include <sys/mman.h>

#ifndef MAP_STACK
#define MAP_STACK 0
#endif

void sb_fiber_prepare(struct sb_fiber_ctx *f,
                      void (*fn)(void *), void *arg)
{
    f->entry_fn = fn;
    f->entry_arg = arg;
    /* AAPCS64 requires SP to be 16-byte aligned at public interfaces. */
    f->regs.sp  = (void *)((uintptr_t)f->stack_end & ~(uintptr_t)0xF);
    f->regs.fp  = 0;
    f->regs.lr  = (void *)SYMBOL(FIBER_TRAMP)->value;
    f->regs.x19 = (void *)f;
    f->regs.x20 = 0;
    f->regs.x21 = 0;
    f->regs.x22 = 0;
    f->regs.x23 = 0;
    f->regs.x24 = 0;
    f->regs.x25 = 0;
    f->regs.x26 = 0;
    f->regs.x27 = 0;
    f->regs.x28 = 0;
    f->regs.d8 = f->regs.d9 = f->regs.d10 = f->regs.d11 = 0.0;
    f->regs.d12 = f->regs.d13 = f->regs.d14 = f->regs.d15 = 0.0;
}

void *sb_fiber_sp(const struct sb_fiber_ctx *f)
{
    return f->regs.sp;
}

int sb_fiber_gc_regs(const struct sb_fiber_ctx *f, lispobj *out, int max)
{
    static const int n = 11;
    if (max < n) return 0;
    int i = 0;
    out[i++] = (lispobj)f->regs.fp;
    out[i++] = (lispobj)f->regs.x19;
    out[i++] = (lispobj)f->regs.x20;
    out[i++] = (lispobj)f->regs.x21;
    out[i++] = (lispobj)f->regs.x22;
    out[i++] = (lispobj)f->regs.x23;
    out[i++] = (lispobj)f->regs.x24;
    out[i++] = (lispobj)f->regs.x25;
    out[i++] = (lispobj)f->regs.x26;
    out[i++] = (lispobj)f->regs.x27;
    out[i++] = (lispobj)f->regs.x28;
    return i;
}

void sb_fiber_rebind_thread(struct sb_fiber_ctx *f, struct thread *new_owner)
{
    f->regs.x21 = (void *)new_owner;
}

int sb_fiber_lisp_stack_alloc(struct sb_fiber_ctx *f, size_t size)
{
    size_t ps = os_reported_page_size;
    size_t guard = STACK_GUARD_SIZE;
    size = ALIGN_UP(size ? size : 65536, ps);
    size_t total = size + 3*guard;
    void *p = mmap(NULL, total, PROT_READ | PROT_WRITE,
                   MAP_PRIVATE | MAP_ANONYMOUS | MAP_STACK, -1, 0);
    if (p == MAP_FAILED) return -1;
    mprotect((char *)p + size + guard,   guard, PROT_NONE);
    mprotect((char *)p + size + 2*guard, guard, PROT_NONE);
    f->control_stack_base       = (lispobj *)p;
    f->control_stack_end        = (lispobj *)((char *)p + total);
    f->control_stack_pointer    = (lispobj *)p;
    f->control_frame_pointer    = NULL;
    f->control_stack_alloc_size = total;
    f->cs_guard_protected       = 1;
    /* Fresh from mmap, so zero: nothing to scrub before the first run. */
    f->stack_dirty              = 0;
    return 0;
}

void sb_fiber_lisp_stack_free(struct sb_fiber_ctx *f)
{
    if (f->control_stack_alloc_size && f->control_stack_base)
        munmap(f->control_stack_base, f->control_stack_alloc_size);
    f->control_stack_base       = NULL;
    f->control_stack_end        = NULL;
    f->control_stack_pointer    = NULL;
    f->control_frame_pointer    = NULL;
    f->control_stack_alloc_size = 0;
}

void sb_fiber_lisp_stack_capture_main(struct sb_fiber_ctx *f, struct thread *th)
{
    f->control_stack_base       = th->control_stack_start;
    f->control_stack_end        = th->control_stack_end;
    f->control_stack_pointer    = th->control_stack_pointer;
    f->control_frame_pointer    = th->control_frame_pointer;
    f->control_stack_alloc_size = 0;   /* not owned */
    f->cs_guard_protected =
        th->state_word.control_stack_guard_page_protected;
    /* The thread's stack has been the running stack at every global
     * collection since its words above the stack pointer were written,
     * or else a main fiber over it was scrubbed on resuming after one
     * (see below), so only collections from now on concern it. */
    f->stack_dirty         = 1;
    f->gc_epoch_seen       = sb_fiber_gc_epoch;
    f->local_gc_epoch_seen = __atomic_load_n(&sb_fiber_local_gc_epoch,
                                             __ATOMIC_ACQUIRE);
    f->heap_gc_count_seen  = 0;
    f->ran_other_heap      = thread_extra_data(th)->heap_on_main_stack;
}

void sb_fiber_lisp_stack_suspend(struct sb_fiber_ctx *f, struct thread *th)
{
    f->control_stack_base    = th->control_stack_start;
    f->control_stack_end     = th->control_stack_end;
    f->control_stack_pointer = th->control_stack_pointer;
    f->control_frame_pointer = th->control_frame_pointer;
    f->cs_guard_protected    =
        th->state_word.control_stack_guard_page_protected;
    f->gc_epoch_seen = sb_fiber_gc_epoch;
#ifdef LISP_FEATURE_SB_LOCAL_HEAPS
    f->heap_gc_count_seen = f->heap ? f->heap->gc_count : 0;
#endif
}

/* The words above a fiber's stack pointer are left over from frames that
 * have returned.  The control stack is scanned precisely, so a frame
 * pushed later that does not initialize every slot before a collection
 * exposes them to the collector as roots.  That is harmless while what
 * they refer to is still allocated, and it stays allocated until the next
 * collection: the collectors scrub the running stack above its stack
 * pointer for this reason (scrub_thread_control_stack), and a stale word
 * written after a collection can only refer to an object that the
 * collection kept or that was allocated after it.
 *
 * A suspended fiber's stack is not the running stack of any thread, so
 * no collection scrubs it.  It is zeroed here, before the fiber runs
 * again, when a collection that could have freed what a stale word refers
 * to has happened since the fiber last ran:
 *
 *  - any global collection;
 *  - a collection of the fiber's own heap, which is possible while the
 *    fiber is suspended only when the heap is installed elsewhere;
 *  - any local collection, for a fiber that has installed a heap other
 *    than its own, since its stack may refer into that heap.  A stack
 *    refers into a heap only if the heap was installed while it ran:
 *    no global object or other heap refers into a heap, and values
 *    crossing a fiber boundary are copied.
 *
 * The extent of the words to zero is not known: a fiber can call deeper
 * than it ever suspends, then return, so no suspend point bounds it, and
 * a run of zero words bounds nothing either, since a frame's unwritten
 * slots are zero.  The whole rest of the stack is zeroed.
 *
 * A fiber that is suspended and resumed without a collection in between
 * pays nothing, whatever depth it suspends at. */
void sb_fiber_lisp_stack_resume(struct sb_fiber_ctx *f, struct thread *th)
{
    uword_t local_epoch = __atomic_load_n(&sb_fiber_local_gc_epoch,
                                          __ATOMIC_ACQUIRE);
    if (f->stack_dirty && f->control_stack_pointer && f->control_stack_end) {
        int collected = f->gc_epoch_seen != sb_fiber_gc_epoch
            || (f->ran_other_heap && f->local_gc_epoch_seen != local_epoch);
#ifdef LISP_FEATURE_SB_LOCAL_HEAPS
        if (f->heap && f->heap->gc_count != f->heap_gc_count_seen)
            collected = 1;
#endif
        char *csp = (char *)f->control_stack_pointer;
        char *usable_end = (char *)f->control_stack_end
                           - 3 * STACK_GUARD_SIZE;
        if (collected && csp < usable_end)
            memset(csp, 0, usable_end - csp);
    }
    f->stack_dirty = 1;
    f->local_gc_epoch_seen = local_epoch;

    th->control_stack_start   = f->control_stack_base;
    th->control_stack_end     = f->control_stack_end;
    th->control_stack_pointer = f->control_stack_pointer;
    th->control_frame_pointer = f->control_frame_pointer;
    th->state_word.control_stack_guard_page_protected = f->cs_guard_protected;
}

size_t sb_fiber_control_stack_size_bytes(const struct sb_fiber_ctx *f)
{
    if (!f->control_stack_base || !f->control_stack_end) return 0;
    size_t total = (char *)f->control_stack_end - (char *)f->control_stack_base;
    if (f->control_stack_alloc_size > 0) {
        size_t guards = 3 * STACK_GUARD_SIZE;
        return total > guards ? total - guards : 0;
    }
    return total;
}

size_t sb_fiber_control_stack_used_bytes(const struct sb_fiber_ctx *f)
{
    if (f->control_stack_pointer && f->control_stack_base)
        return (char *)f->control_stack_pointer - (char *)f->control_stack_base;
    return 0;
}
