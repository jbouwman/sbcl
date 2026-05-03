/*
 * fiber_swap_context(struct fiber_context *save,
 *                    struct fiber_context *restore)
 *
 * Save callee-saved registers + RSP into *save,
 * restore from *restore, return to restore's saved RIP.
 *
 * System V AMD64 ABI callee-saved: RBX, RBP, R12-R15.
 * RDI = save, RSI = restore.
 *
 * The normal Lisp-initiated fiber switch inlines an equivalent
 * sequence via the %fiber-register-swap VOP (see
 * contrib/sb-fiber/x86-64-vops.lisp).  This C stub is kept for
 * fiber_trampoline_c's auto-return path, which runs outside Lisp
 * and cannot call the VOP.
 */
        .text
        .globl  fiber_swap_context
        .type   fiber_swap_context, @function
        .balign 16
fiber_swap_context:
        movq    %rsp,  0x00(%rdi)
        movq    %rbx,  0x08(%rdi)
        movq    %rbp,  0x10(%rdi)
        movq    %r12,  0x18(%rdi)
        movq    %r13,  0x20(%rdi)
        movq    %r14,  0x28(%rdi)
        movq    %r15,  0x30(%rdi)

        movq    0x30(%rsi), %r15
        movq    0x28(%rsi), %r14
        movq    0x20(%rsi), %r13
        movq    0x18(%rsi), %r12
        movq    0x10(%rsi), %rbp
        movq    0x08(%rsi), %rbx
        movq    0x00(%rsi), %rsp

        ret
        .size   fiber_swap_context, . - fiber_swap_context

/*
 * fiber_trampoline_asm
 *
 * Entry point for nascent fibers.  fiber_swap_context's `ret` lands here.
 * R12 holds the fiber pointer (set by sb_fiber_prepare).
 * We move it to RDI (first argument) and call the C trampoline,
 * avoiding fragile inline-asm reads of callee-saved registers.
 */
        .globl  fiber_trampoline_asm
        .type   fiber_trampoline_asm, @function
        .balign 16
fiber_trampoline_asm:
        movq    %r12, %rdi
        call    fiber_trampoline_c
        /* fiber_trampoline_c should never return, but just in case: */
        ud2
        .size   fiber_trampoline_asm, . - fiber_trampoline_asm

#if defined(__linux__) && defined(__ELF__)
        .section .note.GNU-stack,"",%progbits
#endif
