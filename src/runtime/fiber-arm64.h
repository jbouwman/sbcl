#ifndef SBCL_FIBER_ARM64_H
#define SBCL_FIBER_ARM64_H

/* AArch64 fiber register save area.
 * AAPCS64 callee-saved integer set: x19-x28, x29 (FP).
 * x30 (LR) is saved explicitly because the asm does not spill it to
 * the stack the way x86 CALL does; the final RET jumps to the restored
 * x30.  The low 64 bits of v8-v15 (d8-d15) are also callee-saved.
 *
 * Field order is chosen so that the asm can save/restore adjacent pairs
 * with STP/LDP.  Offsets are referenced from fiber_switch_arm64.S and
 * must not change without updating the asm. */
struct fiber_context {
    void   *sp;     /* 0x00 */
    void   *fp;     /* 0x08  x29 */
    void   *lr;     /* 0x10  x30 */
    void   *x19;    /* 0x18 */
    void   *x20;    /* 0x20 */
    void   *x21;    /* 0x28 */
    void   *x22;    /* 0x30 */
    void   *x23;    /* 0x38 */
    void   *x24;    /* 0x40 */
    void   *x25;    /* 0x48 */
    void   *x26;    /* 0x50 */
    void   *x27;    /* 0x58 */
    void   *x28;    /* 0x60 */
    double  d8;     /* 0x68 */
    double  d9;     /* 0x70 */
    double  d10;    /* 0x78 */
    double  d11;    /* 0x80 */
    double  d12;    /* 0x88 */
    double  d13;    /* 0x90 */
    double  d14;    /* 0x98 */
    double  d15;    /* 0xa0 */
};

#endif /* SBCL_FIBER_ARM64_H */
