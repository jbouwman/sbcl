/*
 * fiber_swap_context(struct fiber_context *save,
 *                    struct fiber_context *restore)
 *
 * Save AAPCS64 callee-saved registers into *save, restore them from
 * *restore, and return via the restored x30 (LR).  The layout of
 * struct fiber_context is declared in fiber-arm64.h and the offsets
 * below must stay in sync with it.
 *
 *   0x00 sp       0x50 x26
 *   0x08 fp (x29) 0x58 x27
 *   0x10 lr (x30) 0x60 x28
 *   0x18 x19      0x68 d8
 *   0x20 x20      0x70 d9
 *   0x28 x21      0x78 d10
 *   0x30 x22      0x80 d11
 *   0x38 x23      0x88 d12
 *   0x40 x24      0x90 d13
 *   0x48 x25      0x98 d14
 *                 0xa0 d15
 *
 * x0 = save, x1 = restore (AAPCS64 first two integer args).
 *
 * The Lisp-initiated FIBER-SWITCH path uses the %fiber-register-swap
 * VOP (contrib/sb-fiber/fiber-vops-arm64.lisp), which inlines the
 * equivalent save/load + RET at the call site and skips the alien-
 * funcall round-trip.  This asm stub is still reached by
 * fiber_trampoline_c's auto-return-to-return_fiber path, which runs
 * outside Lisp and therefore cannot use the VOP.
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
        /* Save outgoing context. */
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

        /* Restore incoming context. */
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
 * Entry point for nascent fibers.  fiber_swap_context's final RET
 * branches here because sb_fiber_prepare seeded ctx.lr with this
 * symbol.  X19 holds the fiber pointer (also set by sb_fiber_prepare);
 * we move it to X0 (first C argument) and tail-call the generic C
 * trampoline.
 */
        .globl  GNAME(fiber_trampoline_asm)
        TYPE(GNAME(fiber_trampoline_asm))
        .balign 16
GNAME(fiber_trampoline_asm):
        mov     x0, x19
        bl      GNAME(fiber_trampoline_c)
        /* fiber_trampoline_c should never return, but guard against it. */
        brk     #0
        SIZE(GNAME(fiber_trampoline_asm))

#if defined(__linux__) && defined(__ELF__)
        .section .note.GNU-stack,"",%progbits
#endif
