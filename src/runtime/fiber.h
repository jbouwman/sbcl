#ifndef SBCL_FIBER_H
#define SBCL_FIBER_H

#include "genesis/thread.h"
#include <stddef.h>

/* struct fiber_context is architecture-specific: it must exactly describe
 * the callee-saved register set saved and restored by the per-arch assembly
 * implementation of fiber_swap_context.  The per-arch header below also
 * declares any arch-specific helpers used by fiber.c. */
#if defined(LISP_FEATURE_X86_64)
#  include "fiber-x86-64.h"
#elif defined(LISP_FEATURE_ARM64)
#  include "fiber-arm64.h"
#else
#  error "fiber support not implemented on this architecture"
#endif

/* Fiber states */
enum fiber_state {
    FIBER_NEW  = 0,  /* Created but never switched to */
    FIBER_RUNNABLE = 1,  /* Has been run, currently suspended */
    FIBER_RUNNING  = 2,  /* Currently executing */
    FIBER_DEAD     = 3   /* Finished, destroyed, or on the freelist */
};

/* Per-thread freelist bounds.  sb_fiber_destroy parks a default-sized
 * idle fiber on extra_thread_data(owner)->fiber_freelist instead of
 * munmap'ing its stacks, so back-to-back make/destroy pairs avoid
 * kernel traffic.  Non-default sizes and cross-thread destroy fall
 * through to the real mmap/munmap path. */
#define FIBER_POOL_MAX                   32
#define FIBER_DEFAULT_STACK_SIZE         65536
#define FIBER_DEFAULT_BINDING_STACK_SIZE 8192

struct sb_fiber {
    /* Context switch state */
    struct fiber_context ctx;
    enum fiber_state     state;

    /* Control stack (mmap'd region, guard page at low end) */
    void    *stack_base;        /* lowest address (guard page) */
    void    *stack_start;       /* first usable byte */
    void    *stack_end;         /* one past last usable byte */
    size_t   stack_alloc_size;  /* total mmap'd bytes */

    /* Binding stack (mmap'd, separate region) */
    lispobj *binding_stack_base;
    lispobj *binding_stack_end;
    lispobj *binding_stack_pointer;  /* saved BSP */
    size_t   binding_stack_alloc_size;
    /* Value installed into thread->binding_stack_start (and hence
     * SB-VM:*BINDING-STACK-START*) while this fiber runs.  For child
     * fibers this equals binding_stack_base; for the main fiber it
     * equals the thread's own original binding_stack_start captured
     * at make-main-fiber time. */
    lispobj *binding_stack_start_for_thread;

    /* Per-fiber Lisp control stack bounds.  On arm64 these describe a
     * mmap'd region separate from the native C stack (which the arch
     * tracks via reg_CSP / reg_CFP); the region is owned iff
     * control_stack_alloc_size > 0.  On x86-64 Lisp frames share the
     * native C stack, so base/end just alias stack_start/stack_end
     * and alloc_size stays 0.  Either way the thread-slot installers
     * in sb_fiber_lisp_stack_{suspend,resume} use these bounds to
     * keep thread->control_stack_start/end tracking the running
     * fiber (needed by *CONTROL-STACK-START/END* readers). */
    lispobj *control_stack_base;
    lispobj *control_stack_end;
    lispobj *control_stack_pointer;
    lispobj *control_frame_pointer;
    size_t   control_stack_alloc_size;

    /* Saved thread-struct fields (swapped on fiber_switch) */
    lispobj  current_catch_block;
    lispobj  current_unwind_protect_block;

    /* Per-thread singly-linked list for GC enumeration */
    struct sb_fiber *next;

    /* Owning thread (set at registration) */
    struct thread   *owner;

    /* Entry function and argument (for new fibers) */
    void (*entry_fn)(void *arg);
    void  *entry_arg;

    /* Fiber to auto-switch back to when entry_fn returns.
     * Set by the Lisp fiber-switch shim; the trampoline uses this. */
    struct sb_fiber *return_fiber;

    /* Mirror of th->state_word.control_stack_guard_page_protected,
     * swapped on every fiber switch so the per-thread bit reflects
     * the *running* fiber.  1 = SOFT guard is PROT_NONE (default);
     * 0 = SOFT was lowered by an unhandled-overflow signal and this
     * fiber is running on the freed page while the RETURN guard
     * waits to re-trigger for re-protection. */
    unsigned char cs_guard_protected;

    /* Park/unpark wakeup token.  Orthogonal to the main fiber_state
     * enum above: state tracks execution position (RUNNING/RUNNABLE/
     * ...), park_state tracks scheduler-visible "should this fiber
     * be on the runqueue" with lost-wakeup avoidance.  A parked
     * fiber is still FIBER_RUNNABLE from the switch primitive's
     * perspective; the scheduler is expected to keep it off its
     * runqueue until an unpark flips park_state back to READY. */
    unsigned char park_state;
};

/* park_state values.  Accessed via __atomic_compare_exchange_n. */
#define FIBER_PARK_READY   ((unsigned char) 0)
#define FIBER_PARK_PENDING ((unsigned char) 1)
#define FIBER_PARK_PARKED  ((unsigned char) 2)

/* Lifecycle */
struct sb_fiber *sb_fiber_create(size_t stack_size,
                                 size_t binding_stack_size);
struct sb_fiber *sb_fiber_create_main(struct thread *th);
void             sb_fiber_destroy(struct sb_fiber *fiber);

/* GC registration */
void sb_fiber_register(struct thread *th, struct sb_fiber *fiber);
void sb_fiber_unregister(struct thread *th, struct sb_fiber *fiber);

/* Release every fiber parked on a thread's freelist.  Called from the
 * thread-exit path before the thread struct is released. */
void sb_fiber_pool_drain(struct thread *th);

/* Context-switch prepare.  Callable only from target->owner thread.
 * PREP enters a pseudo-atomic region (setting th->pseudo_atomic_bits
 * to the thread address), installs to's BSP, swaps Lisp-stack
 * bounds, flips states (from RUNNING->RUNNABLE, to -> RUNNING), and
 * performs the binding-stack value swap.  After PREP returns, the
 * caller (the Lisp fiber-switch shim or fiber_trampoline_c's auto-
 * return path) performs the register/SP swap, which transfers
 * execution to TO's saved continuation.
 *
 * EXIT_PA exits the pseudo-atomic region and dispatches any
 * interrupt that was deferred during the region.  On the Lisp path,
 * the %fiber-register-swap VOP emits the equivalent inline asm
 * sequence in its RESUME tail; fiber_trampoline_c calls this entry
 * directly to exit PA at the top of a new fiber. */
void sb_fiber_switch_prep(struct sb_fiber *from, struct sb_fiber *to);
void sb_fiber_exit_pa    (struct thread *th);

/* Park / unpark.  Primitive-level wakeup token; does not itself
 * switch or enqueue.
 *
 *   sb_fiber_park_begin(f)  returns 1 if the caller should now
 *       switch away from f (park_state transitioned READY->PARKED),
 *       0 if a prior unpark had credited PENDING (now consumed,
 *       state is READY and the caller should continue without
 *       switching).  Callable only from f's owner thread (f is
 *       almost always the current fiber).
 *
 *   sb_fiber_unpark(f)      returns 1 if the caller should add f
 *       back to a scheduler runqueue (park_state transitioned
 *       PARKED->READY), 0 if f wasn't parked and a PENDING credit
 *       was stashed for its next park.  Safe to call from signal
 *       handlers and other threads. */
int sb_fiber_park_begin(struct sb_fiber *f);
int sb_fiber_unpark    (struct sb_fiber *f);

/* Stack usage introspection.  Reports the snapshot as of the most
 * recent switch-out (or initial creation) for suspended fibers.  For
 * a RUNNING fiber the binding-stack number is stale; the live value
 * is in the owning thread's binding_stack_pointer and can be obtained
 * via SB-KERNEL:BINDING-STACK-USAGE from within the fiber.  Sizes are
 * the usable capacity, excluding guard pages.  Returns 0 for fields
 * that don't apply to a main fiber (which doesn't own its stacks). */
size_t sb_fiber_binding_stack_usage(const struct sb_fiber *f);
size_t sb_fiber_binding_stack_size (const struct sb_fiber *f);
size_t sb_fiber_control_stack_usage(const struct sb_fiber *f);
size_t sb_fiber_control_stack_size (const struct sb_fiber *f);

/* Byte offsets of struct sb_fiber fields that the Lisp-side switch
 * shim reads and writes via SAP-REF.  Indices are the SB_FIBER_FIELD_*
 * enum in fiber.c.  Returns -1 for unknown indices. */
int sb_fiber_struct_offset(int field);

/* Prepare a new fiber for first switch.  Arch-specific. */
void sb_fiber_prepare(struct sb_fiber *f,
                      void (*fn)(void *), void *arg);

/* Return the saved stack pointer from the fiber's context.
 * Everything from [sp, stack_end) is the suspended live stack region
 * and must be conservatively scanned by the GC.  Arch-specific. */
void *sb_fiber_ctx_sp(const struct sb_fiber *f);

/* Invoke CB on every callee-saved integer register in the fiber's context
 * that may plausibly hold a Lisp object.  Used by the conservative GC to
 * pin objects reachable only through suspended fiber registers.
 * Arch-specific. */
void sb_fiber_ctx_foreach_gc_reg(const struct sb_fiber *f,
                                 void (*cb)(lispobj word, void *arg),
                                 void *arg);

/* --- Per-fiber Lisp control stack hooks (arch-specific) ---
 *
 * On arm64 these manage a fiber-owned mmap'd Lisp control stack
 * separate from the native C stack.  On x86-64 they keep the
 * thread->control_stack_start/end slots in sync with the running
 * fiber's native stack bounds (see fiber-x86-64.c). */

/* Allocate a Lisp control stack region / set control_stack_base/end
 * to the fiber's native stack range.  Returns 0 on success, -1 on
 * failure (arm64 mmap). */
int sb_fiber_lisp_stack_alloc(struct sb_fiber *f, size_t size);

/* Release any Lisp control stack region owned by a destroying fiber. */
void sb_fiber_lisp_stack_free(struct sb_fiber *f);

/* Populate a main-fiber object with a snapshot of the thread's own
 * Lisp control stack bounds so later switches back to main can
 * restore them. */
void sb_fiber_lisp_stack_capture_main(struct sb_fiber *f, struct thread *th);

/* Suspend/resume: save the thread's live Lisp-stack bounds into FROM,
 * then install TO's saved bounds into the thread. */
void sb_fiber_lisp_stack_suspend(struct sb_fiber *from, struct thread *th);
void sb_fiber_lisp_stack_resume (struct sb_fiber *to,   struct thread *th);

/* Enumerate every word on a suspended fiber's Lisp control stack so
 * the GC can preserve objects reachable only through parked frames.
 * No-op on x86-64 (those frames are scanned conservatively as part of
 * the fiber's native C stack). */
void sb_fiber_foreach_lisp_stack_word(const struct sb_fiber *f,
                                      void (*cb)(lispobj word, void *arg),
                                      void *arg);

/* Assembly register/SP swap (called by fiber_trampoline_c's
 * auto-return path and -- on architectures without a %fiber-
 * register-swap VOP -- by any fallback Lisp path).  On x86-64 the
 * normal Lisp fiber-switch inlines the equivalent sequence via the
 * VOP instead of calling through here.  Implemented in
 * fiber_switch_<arch>.S. */
extern void fiber_swap_context(struct fiber_context *save,
                               struct fiber_context *restore);

/* New-fiber entry stub (implemented in per-arch asm).
 * sb_fiber_prepare points the fiber's initial return address here;
 * it transfers to the generic C trampoline. */
extern void fiber_trampoline_asm(void);

/* Generic C trampoline, called from fiber_trampoline_asm. */
void fiber_trampoline_c(struct sb_fiber *self);

#endif /* SBCL_FIBER_H */
