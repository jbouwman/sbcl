#ifndef SBCL_FIBER_X86_64_H
#define SBCL_FIBER_X86_64_H

/* x86-64 fiber register save area.
 * System V AMD64 ABI callee-saved set: RBX, RBP, R12-R15.
 * RSP is saved first and encodes the return address because the asm
 * implementation uses CALL/RET for the switch.
 *
 * Offsets must match fiber_switch_amd64.S. */
struct fiber_context {
    void *rsp;   /* 0x00 */
    void *rbx;   /* 0x08 */
    void *rbp;   /* 0x10 */
    void *r12;   /* 0x18 */
    void *r13;   /* 0x20 */
    void *r14;   /* 0x28 */
    void *r15;   /* 0x30 */
};

#endif /* SBCL_FIBER_X86_64_H */
