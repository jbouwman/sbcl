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
 * contrib/sb-fiber/fiber-vops-x86-64.lisp).  This C stub is kept for
 * fiber_trampoline_c's auto-return path, which runs outside Lisp
 * and cannot call the VOP.
 */

/*
 * Darwin's linker prefixes C symbols with underscore; ELF doesn't.
 * Mach-O and COFF also reject GAS's .type / .size directives.
 * Mirror the scheme used in x86-64-assem.S.
 */
#if defined(__APPLE__) || defined(__MACH__)
# define GNAME(name) _##name
#else
# define GNAME(name) name
#endif

#if !defined(__APPLE__) && !defined(__MACH__) && !defined(_WIN32)
# define TYPE(name) .type name,@function
# define SIZE(name) .size name,.-name
#else
# define TYPE(name)
# define SIZE(name)
#endif

        .text
        .globl  GNAME(fiber_swap_context)
        TYPE(GNAME(fiber_swap_context))
        .balign 16
GNAME(fiber_swap_context):
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
        SIZE(GNAME(fiber_swap_context))

/*
 * fiber_trampoline_asm
 *
 * Entry point for new fibers.  fiber_swap_context's `ret` lands here.
 * R12 holds the fiber pointer (set by sb_fiber_prepare).
 * We move it to RDI (first argument) and call the C trampoline,
 * avoiding fragile inline-asm reads of callee-saved registers.
 */
        .globl  GNAME(fiber_trampoline_asm)
        TYPE(GNAME(fiber_trampoline_asm))
        .balign 16
GNAME(fiber_trampoline_asm):
        movq    %r12, %rdi
        call    GNAME(fiber_trampoline_c)
        /* fiber_trampoline_c should never return, but just in case: */
        ud2
        SIZE(GNAME(fiber_trampoline_asm))

#if defined(__linux__) && defined(__ELF__)
        .section .note.GNU-stack,"",%progbits
#endif
