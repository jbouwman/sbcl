#include "fiber.h"
#include "os.h"
#include "thread.h"
#include "validate.h"
#include <stdint.h>
#include <sys/mman.h>

#ifndef MAP_STACK
#define MAP_STACK 0
#endif

/* Prepare a new fiber for first switch.  The AArch64 asm does not
 * use CALL/RET with a saved return address on the stack; instead
 * fiber_swap_context restores x30 (LR) directly and the final RET
 * branches to it.  So we point LR at fiber_trampoline_asm and stash
 * the fiber pointer in x19 (callee-saved) for the stub to read. */
void sb_fiber_prepare(struct sb_fiber *f,
                      void (*fn)(void *), void *arg)
{
    f->entry_fn = fn;
    f->entry_arg = arg;

    /* AAPCS64 requires SP to be 16-byte aligned at public interfaces. */
    uintptr_t sp = ((uintptr_t)f->stack_end) & ~(uintptr_t)0xF;

    f->ctx.sp  = (void *)sp;
    f->ctx.fp  = 0;
    f->ctx.lr  = (void *)fiber_trampoline_asm;
    f->ctx.x19 = (void *)f;      /* fiber self-ptr, read by the asm stub */
    f->ctx.x20 = 0;
    f->ctx.x21 = 0;
    f->ctx.x22 = 0;
    f->ctx.x23 = 0;
    f->ctx.x24 = 0;
    f->ctx.x25 = 0;
    f->ctx.x26 = 0;
    f->ctx.x27 = 0;
    f->ctx.x28 = 0;
    f->ctx.d8  = 0.0;
    f->ctx.d9  = 0.0;
    f->ctx.d10 = 0.0;
    f->ctx.d11 = 0.0;
    f->ctx.d12 = 0.0;
    f->ctx.d13 = 0.0;
    f->ctx.d14 = 0.0;
    f->ctx.d15 = 0.0;
}

void *sb_fiber_ctx_sp(const struct sb_fiber *f)
{
    return f->ctx.sp;
}

/* Enumerate callee-saved integer registers that may hold a Lisp pointer.
 * SP is the conservative scan range lower bound (handled separately),
 * LR always holds a code address (not a Lisp object), and the d-regs
 * carry raw float payloads. */
void sb_fiber_ctx_foreach_gc_reg(const struct sb_fiber *f,
                                 void (*cb)(lispobj, void *),
                                 void *arg)
{
    cb((lispobj)f->ctx.fp,  arg);
    cb((lispobj)f->ctx.x19, arg);
    cb((lispobj)f->ctx.x20, arg);
    cb((lispobj)f->ctx.x21, arg);
    cb((lispobj)f->ctx.x22, arg);
    cb((lispobj)f->ctx.x23, arg);
    cb((lispobj)f->ctx.x24, arg);
    cb((lispobj)f->ctx.x25, arg);
    cb((lispobj)f->ctx.x26, arg);
    cb((lispobj)f->ctx.x27, arg);
    cb((lispobj)f->ctx.x28, arg);
}

/* --- Per-fiber Lisp control stack ---
 *
 * arm64 SBCL tracks Lisp frames on a stack distinct from the native
 * C stack.  It is rooted in thread->control_stack_{start,end} with
 * the live pointer saved to thread->control_stack_pointer on every
 * transition from Lisp to C.  When a child fiber's alien callback
 * enters Lisp for the first time, call_into_lisp loads reg_CSP from
 * thread->control_stack_pointer -- so that field must already refer
 * to the fiber's own region.  Multiple fibers sharing the thread's
 * single Lisp control stack would silently overwrite each other's
 * frames on interleaved switches.
 *
 * Each fiber therefore owns an mmap'd region and the thread struct's
 * control_stack_{start,end,pointer,frame_pointer} fields are swapped
 * in and out on every switch.  The main fiber "owns" the thread's
 * original region (no allocation) and simply restores the thread's
 * view to what it was at make_main_fiber time. */

static size_t align_up_local(size_t v, size_t a) {
    return (v + a - 1) & ~(a - 1);
}

int sb_fiber_lisp_stack_alloc(struct sb_fiber *f, size_t size)
{
    size_t ps = os_reported_page_size;
    size_t guard = STACK_GUARD_SIZE;
    size = align_up_local(size ? size : 65536, ps);
    /* Allocate 3 extra STACK_GUARD_SIZE regions at the top for the
     * RETURN/SOFT/HARD guards.  arm64's Lisp control stack grows
     * upward, so overflow proceeds from low to high addresses
     * through RETURN (R+W), SOFT (PROT_NONE), HARD (PROT_NONE).
     * Layout lines up with the validate.h macros for the upward
     * case:
     *   HARD   = control_stack_end - G
     *   SOFT   = HARD - G
     *   RETURN = SOFT - G
     *
     * G = STACK_GUARD_SIZE.  See the note in fiber.c::sb_fiber_create
     * on why this is not necessarily os_reported_page_size.
     */
    size_t total = size + 3*guard;
    void *p = mmap(NULL, total, PROT_READ | PROT_WRITE,
                   MAP_PRIVATE | MAP_ANONYMOUS | MAP_STACK, -1, 0);
    if (p == MAP_FAILED) return -1;
    /* Guards at the top.  RETURN stays R+W; SOFT and HARD PROT_NONE. */
    mprotect((char *)p + size + guard,   guard, PROT_NONE);   /* SOFT */
    mprotect((char *)p + size + 2*guard, guard, PROT_NONE);   /* HARD */
    f->control_stack_base       = (lispobj *)p;
    f->control_stack_end        = (lispobj *)((char *)p + total);
    /* arm64's Lisp control stack grows upward from base.  The first
     * use (by call_into_lisp) starts writing at control_stack_pointer
     * and advances. */
    f->control_stack_pointer    = (lispobj *)p;
    f->control_frame_pointer    = (lispobj *)p;
    f->control_stack_alloc_size = total;
    f->cs_guard_protected       = 1;
    return 0;
}

void sb_fiber_lisp_stack_free(struct sb_fiber *f)
{
    if (f->control_stack_alloc_size && f->control_stack_base) {
        munmap(f->control_stack_base, f->control_stack_alloc_size);
    }
    f->control_stack_base       = NULL;
    f->control_stack_end        = NULL;
    f->control_stack_pointer    = NULL;
    f->control_frame_pointer    = NULL;
    f->control_stack_alloc_size = 0;
}

void sb_fiber_lisp_stack_capture_main(struct sb_fiber *f, struct thread *th)
{
    /* Main fiber: describe the thread's own stack; no allocation. */
    f->control_stack_base       = th->control_stack_start;
    f->control_stack_end        = th->control_stack_end;
    f->control_stack_pointer    = th->control_stack_pointer;
    f->control_frame_pointer    = th->control_frame_pointer;
    f->control_stack_alloc_size = 0;   /* not owned */
    f->cs_guard_protected =
        th->state_word.control_stack_guard_page_protected;
}

void sb_fiber_lisp_stack_suspend(struct sb_fiber *f, struct thread *th)
{
    /* Save live thread view into the outgoing fiber record. */
    f->control_stack_base    = th->control_stack_start;
    f->control_stack_end     = th->control_stack_end;
    f->control_stack_pointer = th->control_stack_pointer;
    f->control_frame_pointer = th->control_frame_pointer;
    /* The per-thread guard-protection bit belongs to the fiber
     * whose stack it describes; save it so a later resume restores
     * the right bit. */
    f->cs_guard_protected    = th->state_word.control_stack_guard_page_protected;
}

void sb_fiber_lisp_stack_resume(struct sb_fiber *f, struct thread *th)
{
    /* Install the incoming fiber's saved Lisp-stack bounds on the
     * thread struct.  This must happen before any Lisp callback runs
     * in the resumed fiber. */
    th->control_stack_start   = f->control_stack_base;
    th->control_stack_end     = f->control_stack_end;
    th->control_stack_pointer = f->control_stack_pointer;
    th->control_frame_pointer = f->control_frame_pointer;
    th->state_word.control_stack_guard_page_protected = f->cs_guard_protected;
}

void sb_fiber_foreach_lisp_stack_word(const struct sb_fiber *f,
                                      void (*cb)(lispobj, void *),
                                      void *arg)
{
    /* The live span of a suspended fiber's Lisp control stack runs
     * from the base to the saved stack pointer.  A fiber that has
     * never entered Lisp has pointer == base and there is nothing
     * to scan. */
    lispobj *lo = f->control_stack_base;
    lispobj *hi = f->control_stack_pointer;
    if (!lo || !hi || hi <= lo) return;
    for (lispobj *p = lo; p < hi; p++) cb(*p, arg);
}
