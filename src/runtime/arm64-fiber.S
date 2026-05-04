/*
 * fiber_swap_context(struct fiber_context *save,
 *                    struct fiber_context *restore)
 *
 * Save AAPCS64 callee-saved registers + SP into *save, restore from
 * *restore, return via the restored x30 (LR).  Offsets match
 * struct fiber_context in arm64-fiber.h.  x0 = save, x1 = restore.
 *
 * The Lisp-initiated FIBER-SWITCH path inlines an equivalent
 * sequence via the %fiber-register-swap VOP (see
 * contrib/sb-fiber/arm64-vops.lisp).  This stub is kept for
 * fiber_trampoline_c's auto-return path, which runs outside Lisp
 * and cannot call the VOP.
 */

#ifdef __APPLE__
#define GNAME(var)   _##var
#define TYPE(name)
#define SIZE(name)
#else
#define GNAME(var)   var
#define TYPE(name)   .type name, %function
#define SIZE(name)   .size name, . - name
#endif

        .text
        .globl  GNAME(fiber_swap_context)
        TYPE(GNAME(fiber_swap_context))
        .balign 16
GNAME(fiber_swap_context):
        mov     x9, sp
        str     x9,        [x0, #0x00]
        stp     x29, x30,  [x0, #0x08]
        stp     x19, x20,  [x0, #0x18]
        stp     x21, x22,  [x0, #0x28]
        stp     x23, x24,  [x0, #0x38]
        stp     x25, x26,  [x0, #0x48]
        stp     x27, x28,  [x0, #0x58]
        stp     d8,  d9,   [x0, #0x68]
        stp     d10, d11,  [x0, #0x78]
        stp     d12, d13,  [x0, #0x88]
        stp     d14, d15,  [x0, #0x98]

        ldr     x9,        [x1, #0x00]
        mov     sp, x9
        ldp     x29, x30,  [x1, #0x08]
        ldp     x19, x20,  [x1, #0x18]
        ldp     x21, x22,  [x1, #0x28]
        ldp     x23, x24,  [x1, #0x38]
        ldp     x25, x26,  [x1, #0x48]
        ldp     x27, x28,  [x1, #0x58]
        ldp     d8,  d9,   [x1, #0x68]
        ldp     d10, d11,  [x1, #0x78]
        ldp     d12, d13,  [x1, #0x88]
        ldp     d14, d15,  [x1, #0x98]

        ret
        SIZE(GNAME(fiber_swap_context))

/*
 * fiber_trampoline_asm
 *
 * Entry point for nascent fibers.  fiber_swap_context's RET lands
 * here because sb_fiber_prepare seeded ctx.lr with this symbol.
 * x19 holds the fiber pointer (also seeded by sb_fiber_prepare);
 * move it into x0 and call the C trampoline.
 */
        .globl  GNAME(fiber_trampoline_asm)
        TYPE(GNAME(fiber_trampoline_asm))
        .balign 16
GNAME(fiber_trampoline_asm):
        mov     x0, x19
        bl      GNAME(fiber_trampoline_c)
        /* fiber_trampoline_c should never return, but just in case: */
        brk     #0
        SIZE(GNAME(fiber_trampoline_asm))

#if defined(__linux__) && defined(__ELF__)
        .section .note.GNU-stack,"",%progbits
#endif
