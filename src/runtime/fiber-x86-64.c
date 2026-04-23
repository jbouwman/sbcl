#include "fiber.h"
#include <stdint.h>

/* Prepare a new fiber.  Set up a fake return frame so that the
 * first RET inside fiber_swap_context lands on fiber_trampoline_asm,
 * which in turn calls fiber_trampoline_c(self).  The fiber pointer is
 * stashed in a callee-saved register (R12) so the asm stub can find it
 * without reading memory. */
void sb_fiber_prepare(struct sb_fiber *f,
                      void (*fn)(void *), void *arg)
{
    f->entry_fn = fn;
    f->entry_arg = arg;

    void **sp = (void **)f->stack_end;
    sp = (void **)((uintptr_t)sp & ~0xFULL);   /* 16-byte align */
    *(--sp) = (void *)0;                        /* padding / fake retaddr */
    *(--sp) = (void *)fiber_trampoline_asm;     /* RET target */

    f->ctx.rsp = sp;
    f->ctx.rbp = 0;
    f->ctx.rbx = 0;
    f->ctx.r12 = (void *)f;                     /* fiber self-ptr */
    f->ctx.r13 = 0;
    f->ctx.r14 = 0;
    f->ctx.r15 = 0;
}

void *sb_fiber_ctx_sp(const struct sb_fiber *f)
{
    return f->ctx.rsp;
}

void sb_fiber_ctx_foreach_gc_reg(const struct sb_fiber *f,
                                 void (*cb)(lispobj, void *),
                                 void *arg)
{
    cb((lispobj)f->ctx.rbx, arg);
    cb((lispobj)f->ctx.rbp, arg);
    cb((lispobj)f->ctx.r12, arg);
    cb((lispobj)f->ctx.r13, arg);
    cb((lispobj)f->ctx.r14, arg);
    cb((lispobj)f->ctx.r15, arg);
}

/* On x86-64 Lisp frames live on the native C stack -- the register/SP
 * swap done by fiber_swap_context already moves them.  But the thread
 * struct also publishes thread->control_stack_start/end (exposed to
 * Lisp as *CONTROL-STACK-START* / *CONTROL-STACK-END*), and that range
 * is consulted by SB-EXT:STACK-ALLOCATED-P and the AVER in
 * SB-KERNEL::COPY-CTYPE, among others.  If we leave it pointing at the
 * main OS thread's stack, every DX allocation on a fiber's stack looks
 * "not stack-allocated," and the AVER fires.
 *
 * The per-fiber Lisp-stack hooks are the natural place to fix this:
 * each fiber carries its stack bounds in control_stack_base/_end,
 * lisp_stack_resume installs them into the thread on switch-in, and
 * lisp_stack_suspend pulls the current values back out on switch-out.
 * No separate mmap -- the bounds just alias the native C stack range. */

int sb_fiber_lisp_stack_alloc(struct sb_fiber *f, size_t size)
{
    (void)size;
    /* control_stack_base points at the HARD guard page (the low-
     * address end of the stack region on this arch), NOT the first
     * usable byte.  SBCL's per-thread guard-page macros derive
     * HARD/SOFT/RETURN guard addresses from th->control_stack_start
     * as HARD, HARD+ps, HARD+2ps; with base set to stack_base the
     * macros land on the three guard pages allocated in
     * sb_fiber_create, so handle_guard_page_triggered recognizes a
     * fiber overflow as an ordinary control-stack overflow. */
    f->control_stack_base = (lispobj *)f->stack_base;
    f->control_stack_end  = (lispobj *)f->stack_end;
    return 0;
}

void sb_fiber_lisp_stack_free(struct sb_fiber *f)
{
    f->control_stack_base = NULL;
    f->control_stack_end  = NULL;
}

void sb_fiber_lisp_stack_capture_main(struct sb_fiber *f, struct thread *th)
{
    /* Main fiber -- stacks aren't owned; just snapshot the thread's
     * live bounds so a later switch back to main can restore them. */
    f->control_stack_base = th->control_stack_start;
    f->control_stack_end  = th->control_stack_end;
    f->cs_guard_protected =
        th->state_word.control_stack_guard_page_protected;
}

void sb_fiber_lisp_stack_suspend(struct sb_fiber *f, struct thread *th)
{
    f->control_stack_base = th->control_stack_start;
    f->control_stack_end  = th->control_stack_end;
    /* Record the thread's per-stack guard-protection bit in the
     * outgoing fiber so a later resume can restore it (the bit is
     * kept in th->state_word, not in any per-fiber register). */
    f->cs_guard_protected = th->state_word.control_stack_guard_page_protected;
}

void sb_fiber_lisp_stack_resume(struct sb_fiber *f, struct thread *th)
{
    th->control_stack_start = f->control_stack_base;
    th->control_stack_end   = f->control_stack_end;
    th->state_word.control_stack_guard_page_protected = f->cs_guard_protected;
}

void sb_fiber_foreach_lisp_stack_word(const struct sb_fiber *f,
                                      void (*cb)(lispobj, void *),
                                      void *arg)
{
    (void)f; (void)cb; (void)arg;
}
