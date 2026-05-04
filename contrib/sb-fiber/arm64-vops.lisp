;;;; Inline register/SP swap for FIBER-SWITCH (arm64).
;;;;
;;;; AArch64 has no on-stack return address: instead of pushing
;;;; RESUME below SP we ADR its PC-relative address into x30 and
;;;; save it in ctx.lr.  The terminal RET consumes the incoming
;;;; fiber's ctx.lr -- RESUME of an earlier suspension, or
;;;; fiber_trampoline_asm seeded by sb_fiber_prepare for a new fiber.

(in-package "SB-VM")

(sb-c:defknown sb-fiber::%fiber-register-swap
    (system-area-pointer system-area-pointer) (values)
    (sb-c:always-translatable))

(define-vop (fiber-register-swap)
  (:translate sb-fiber::%fiber-register-swap)
  (:policy :fast-safe)
  (:args (from :scs (sap-reg) :target from-tmp)
         (to   :scs (sap-reg) :target to-tmp))
  (:arg-types system-area-pointer system-area-pointer)
  ;; Pin temps to caller-saved regs (x0/x1/x2); the load sequence
  ;; below rewrites every callee-saved GPR, so to-tmp must not alias
  ;; one of them.
  (:temporary (:sc sap-reg :offset nl0-offset
               :from (:argument 0) :to (:result 0))
              from-tmp)
  (:temporary (:sc sap-reg :offset nl1-offset
               :from (:argument 1) :to (:result 0))
              to-tmp)
  (:temporary (:sc unsigned-reg :offset nl2-offset) temp)
  (:generator 10
    (move from-tmp from)
    (move to-tmp   to)
    (let ((x19 (make-random-tn (sc-or-lose 'unsigned-reg) r9-offset))
          (x20 (make-random-tn (sc-or-lose 'unsigned-reg) r10-offset))
          (nfp (make-random-tn (sc-or-lose 'any-reg) nfp-offset))
          (d8  (make-random-tn (sc-or-lose 'double-reg) 8))
          (d9  (make-random-tn (sc-or-lose 'double-reg) 9))
          (d10 (make-random-tn (sc-or-lose 'double-reg) 10))
          (d11 (make-random-tn (sc-or-lose 'double-reg) 11))
          (d12 (make-random-tn (sc-or-lose 'double-reg) 12))
          (d13 (make-random-tn (sc-or-lose 'double-reg) 13))
          (d14 (make-random-tn (sc-or-lose 'double-reg) 14))
          (d15 (make-random-tn (sc-or-lose 'double-reg) 15)))
      ;; SP can't be source/dest of STR (reg 31 decodes to XZR);
      ;; bounce it through TEMP.
      (inst mov-sp temp nsp-tn)
      (inst str temp (@ from-tmp 0))
      ;; Manufacture ctx.lr = RESUME.  Clobbering x30 here is fine:
      ;; the prologue already spilled the caller's return address.
      (inst adr lr-tn RESUME)
      ;; Save callee-saved GPRs (offsets match struct fiber_context
      ;; in arm64-fiber.h).
      (inst stp cfp-tn    lr-tn        (@ from-tmp #x08))
      (inst stp x19       x20          (@ from-tmp #x18))
      (inst stp thread-tn lexenv-tn    (@ from-tmp #x28))
      (inst stp nargs-tn  nfp          (@ from-tmp #x38))
      (inst stp ocfp-tn   null-tn      (@ from-tmp #x48))
      (inst stp csp-tn    cardtable-tn (@ from-tmp #x58))
      (inst stp d8  d9  (@ from-tmp #x68))
      (inst stp d10 d11 (@ from-tmp #x78))
      (inst stp d12 d13 (@ from-tmp #x88))
      (inst stp d14 d15 (@ from-tmp #x98))
      ;; Load the incoming context, SP last so to-tmp stays valid.
      (inst ldp cfp-tn    lr-tn        (@ to-tmp #x08))
      (inst ldp x19       x20          (@ to-tmp #x18))
      (inst ldp thread-tn lexenv-tn    (@ to-tmp #x28))
      (inst ldp nargs-tn  nfp          (@ to-tmp #x38))
      (inst ldp ocfp-tn   null-tn      (@ to-tmp #x48))
      (inst ldp csp-tn    cardtable-tn (@ to-tmp #x58))
      (inst ldp d8  d9  (@ to-tmp #x68))
      (inst ldp d10 d11 (@ to-tmp #x78))
      (inst ldp d12 d13 (@ to-tmp #x88))
      (inst ldp d14 d15 (@ to-tmp #x98))
      (inst ldr temp (@ to-tmp 0))
      (inst mov-sp nsp-tn temp))
    ;; RET branches to x30 (RESUME, or trampoline for a new fiber).
    (inst ret)
    ;; Resume tail: PA is still set from prep -- exit it now (clear
    ;; PA_IN low half, trap on PA_INTERRUPTED high half).
    RESUME
    (inst dmb :ishst)
    (inst str wzr-tn
          (@ thread-tn (* n-word-bytes thread-pseudo-atomic-bits-slot)))
    (inst ldr (32-bit-reg temp)
          (@ thread-tn (+ (* n-word-bytes thread-pseudo-atomic-bits-slot) 4)))
    (let ((not-interrupted (gen-label)))
      (inst cbz temp not-interrupted)
      (inst brk pending-interrupt-trap)
      (emit-label not-interrupted))))
