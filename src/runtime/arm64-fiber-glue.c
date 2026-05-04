#include "fiber.h"
#include "os.h"
#include "thread.h"
#include "validate.h"
#include <stdint.h>
#include <string.h>
#include <sys/mman.h>

#ifndef MAP_STACK
#define MAP_STACK 0
#endif

static size_t align_up_local(size_t v, size_t a) {
    return (v + a - 1) & ~(a - 1);
}

/* Seed a new fiber's saved context: stack-aligned SP, ctx.lr ->
 * fiber_trampoline_asm so the first RET out of fiber_swap_context
 * (or the %fiber-register-swap VOP) lands there, fiber pointer in
 * x19 (callee-saved) for the asm stub to read. */
void sb_fiber_prepare(struct sb_fiber *f,
                      void (*fn)(void *), void *arg)
{
    f->entry_fn = fn;
    f->entry_arg = arg;
    /* AAPCS64 requires SP to be 16-byte aligned at public interfaces. */
    f->ctx.sp  = (void *)((uintptr_t)f->stack_end & ~(uintptr_t)0xF);
    f->ctx.fp  = 0;
    f->ctx.lr  = (void *)fiber_trampoline_asm;
    f->ctx.x19 = (void *)f;
    f->ctx.x20 = 0;
    f->ctx.x21 = 0;
    f->ctx.x22 = 0;
    f->ctx.x23 = 0;
    f->ctx.x24 = 0;
    f->ctx.x25 = 0;
    f->ctx.x26 = 0;
    f->ctx.x27 = 0;
    f->ctx.x28 = 0;
    f->ctx.d8 = f->ctx.d9 = f->ctx.d10 = f->ctx.d11 = 0.0;
    f->ctx.d12 = f->ctx.d13 = f->ctx.d14 = f->ctx.d15 = 0.0;
}

void *sb_fiber_ctx_sp(const struct sb_fiber *f)
{
    return f->ctx.sp;
}

/* Callee-saved integer registers that may hold a Lisp pointer.  SP
 * bounds the conservative scan range (handled separately); LR is a
 * code address; d-regs carry float payloads. */
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
 * arm64 SBCL keeps Lisp frames on a stack distinct from the native
 * C stack: thread->control_stack_{start,end,pointer,frame_pointer}.
 * Each fiber owns an mmap'd region for it, swapped into the thread
 * struct on every switch.  The main fiber doesn't allocate -- it
 * snapshots the thread's own region at make-main-fiber time. */

int sb_fiber_lisp_stack_alloc(struct sb_fiber *f, size_t size)
{
    size_t ps = os_reported_page_size;
    size_t guard = STACK_GUARD_SIZE;
    size = align_up_local(size ? size : 65536, ps);
    /* arm64's Lisp control stack grows upward; place
     * RETURN/SOFT/HARD guards at the high-address end so the
     * validate.h guard-derivation macros land on them. */
    size_t total = size + 3*guard;
    void *p = mmap(NULL, total, PROT_READ | PROT_WRITE,
                   MAP_PRIVATE | MAP_ANONYMOUS | MAP_STACK, -1, 0);
    if (p == MAP_FAILED) return -1;
    /* RETURN stays R+W; SOFT and HARD PROT_NONE. */
    mprotect((char *)p + size + guard,   guard, PROT_NONE);
    mprotect((char *)p + size + 2*guard, guard, PROT_NONE);
    f->control_stack_base       = (lispobj *)p;
    f->control_stack_end        = (lispobj *)((char *)p + total);
    /* control_frame_pointer must be NULL (not base) so the bottom-
     * most Lisp frame's saved OCFP slot is written as 0, terminating
     * the GC frame-chain walker.  Mirrors alloc.c for a fresh
     * thread. */
    f->control_stack_pointer    = (lispobj *)p;
    f->control_frame_pointer    = NULL;
    f->control_stack_alloc_size = total;
    f->cs_guard_protected       = 1;
    return 0;
}

void sb_fiber_lisp_stack_free(struct sb_fiber *f)
{
    if (f->control_stack_alloc_size && f->control_stack_base)
        munmap(f->control_stack_base, f->control_stack_alloc_size);
    f->control_stack_base       = NULL;
    f->control_stack_end        = NULL;
    f->control_stack_pointer    = NULL;
    f->control_frame_pointer    = NULL;
    f->control_stack_alloc_size = 0;
}

void sb_fiber_lisp_stack_capture_main(struct sb_fiber *f, struct thread *th)
{
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
    f->control_stack_base    = th->control_stack_start;
    f->control_stack_end     = th->control_stack_end;
    f->control_stack_pointer = th->control_stack_pointer;
    f->control_frame_pointer = th->control_frame_pointer;
    f->cs_guard_protected    =
        th->state_word.control_stack_guard_page_protected;
    /* Scrub stale data above the saved CSP: scavenge_control_stack
     * scans every word [start, pointer) conservatively, so a future
     * resume must not find pointer-shaped residue from a prior tenant
     * in slots a fresh prologue's `add CSP, ...` reserves uninited.
     * SOFT and HARD guards are PROT_NONE, so stop before them. */
    if (f->control_stack_pointer && f->control_stack_end) {
        char *usable_end = (char *)f->control_stack_end
                           - 3 * STACK_GUARD_SIZE;
        if ((char *)f->control_stack_pointer < usable_end)
            memset(f->control_stack_pointer, 0,
                   usable_end - (char *)f->control_stack_pointer);
    }
}

void sb_fiber_lisp_stack_resume(struct sb_fiber *f, struct thread *th)
{
    th->control_stack_start   = f->control_stack_base;
    th->control_stack_end     = f->control_stack_end;
    th->control_stack_pointer = f->control_stack_pointer;
    th->control_frame_pointer = f->control_frame_pointer;
    th->state_word.control_stack_guard_page_protected = f->cs_guard_protected;
}

/* Conservatively scan a suspended fiber's Lisp control stack so GC
 * can preserve objects reachable only through parked frames.  A
 * fiber that never entered Lisp has pointer == base and is empty. */
void sb_fiber_foreach_lisp_stack_word(const struct sb_fiber *f,
                                      void (*cb)(lispobj, void *),
                                      void *arg)
{
    lispobj *lo = f->control_stack_base;
    lispobj *hi = f->control_stack_pointer;
    if (!lo || !hi || hi <= lo) return;
    for (lispobj *p = lo; p < hi; p++) cb(*p, arg);
}
