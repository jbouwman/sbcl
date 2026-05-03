#ifndef SBCL_FIBER_H
#define SBCL_FIBER_H

#include "genesis/thread.h"
#include <stddef.h>

/* struct fiber_context describes the callee-saved register set
 * saved/restored by fiber_swap_context. */
#if defined(LISP_FEATURE_X86_64)
#  include "x86-64-fiber.h"
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

/* See README.md "Freelist". */
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
    /* Value to install into thread->binding_stack_start while this
     * fiber runs.  Equals binding_stack_base for child fibers; for
     * the main fiber, equals the thread's original. */
    lispobj *binding_stack_start_for_thread;

    /* Native C stack range to install in thread->control_stack_*
     * while this fiber runs (for *CONTROL-STACK-START/END*). */
    lispobj *control_stack_base;
    lispobj *control_stack_end;

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

    /* CS / BS guard state mirrors of the thread's
     * state_word.{control,binding}_stack_guard_page_protected bits.
     * 1 = SOFT armed; 0 = SOFT lowered, RETURN armed in its place. */
    unsigned char cs_guard_protected;
    unsigned char bs_guard_protected;
};

/* Lifecycle */
struct sb_fiber *sb_fiber_create(size_t stack_size,
                                 size_t binding_stack_size);
struct sb_fiber *sb_fiber_create_main(struct thread *th);
void             sb_fiber_destroy(struct sb_fiber *fiber);

/* GC registration */
void sb_fiber_register(struct thread *th, struct sb_fiber *fiber);
void sb_fiber_unregister(struct thread *th, struct sb_fiber *fiber);

/* Drain a thread's freelist on thread exit. */
void sb_fiber_pool_drain(struct thread *th);

/* Release every fiber still on a thread's fiber_list (i.e., that the
 * user did not DESTROY-FIBER).  Called from free_thread_struct so a
 * thread exit doesn't leak each registered fiber's mmap'd stacks. */
void sb_fiber_release_registered(struct thread *th);

/* BS guard manipulation, mirroring the thread-side lower_/reset_ pair. */
void sb_fiber_lower_bs_guard(struct sb_fiber *f);
void sb_fiber_reset_bs_guard(struct sb_fiber *f);

/* Classify a faulting address against fiber binding-stack guards.
 * Returns 0 (no match), 1 (HARD: lose), 2 (SOFT: lowered, raise
 * BINDING-STACK-EXHAUSTED-ERROR), or 3 (RETURN: reset, return). */
int sb_fiber_classify_bs_fault(struct thread *th, void *addr,
                               struct sb_fiber **out_fiber);

/* Context switch.  prep enters PA, swaps BSP/Lisp-stack bounds and
 * state, runs the binding-stack swap; the caller (VOP or
 * fiber_swap_context) performs the register/SP swap.  exit_pa exits
 * PA and dispatches any deferred interrupt.  See README.md
 * "Pseudo-atomic switch window". */
void sb_fiber_switch_prep(struct sb_fiber *from, struct sb_fiber *to);
void sb_fiber_exit_pa    (struct thread *th);

/* Byte offset of an sb_fiber field.  FIELD is one of the
 * SB_FIBER_FIELD_* values in fiber.c.  Returns -1 if unknown.  Used
 * by the Lisp shim to query layout at load time. */
int sb_fiber_struct_offset(int field);

/* Arch-specific helpers (x86-64-fiber-glue.c). */
void  sb_fiber_prepare(struct sb_fiber *f, void (*fn)(void *), void *arg);
void *sb_fiber_ctx_sp (const struct sb_fiber *f);
void  sb_fiber_ctx_foreach_gc_reg(const struct sb_fiber *f,
                                  void (*cb)(lispobj word, void *arg),
                                  void *arg);

/* Per-fiber Lisp control stack hooks (x86-64-fiber-glue.c). */
int  sb_fiber_lisp_stack_alloc       (struct sb_fiber *f, size_t size);
void sb_fiber_lisp_stack_free        (struct sb_fiber *f);
void sb_fiber_lisp_stack_capture_main(struct sb_fiber *f, struct thread *th);
void sb_fiber_lisp_stack_suspend     (struct sb_fiber *f, struct thread *th);
void sb_fiber_lisp_stack_resume      (struct sb_fiber *f, struct thread *th);

/* Asm fallback for register/SP swap (x86-64-fiber.S).  Used by
 * fiber_trampoline_c's auto-return path; Lisp switches use the VOP. */
extern void fiber_swap_context(struct fiber_context *save,
                               struct fiber_context *restore);
extern void fiber_trampoline_asm(void);
void fiber_trampoline_c(struct sb_fiber *self);

#endif /* SBCL_FIBER_H */
