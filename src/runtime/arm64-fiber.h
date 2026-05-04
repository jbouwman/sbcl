#ifndef SBCL_FIBER_ARM64_H
#define SBCL_FIBER_ARM64_H

/* arm64 fiber register save area.
 * AAPCS64 callee-saved set: x19-x28, x29 (FP), and the low halves
 * of v8-v15 (d8-d15).  x30 (LR) is saved explicitly because, unlike
 * x86 CALL, arm64 BL does not push the return address onto the
 * stack -- the final RET branches to the restored x30 directly.
 *
 * Field order is chosen so the asm can save/restore adjacent pairs
 * with STP/LDP.  Offsets must match arm64-fiber.S. */
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
