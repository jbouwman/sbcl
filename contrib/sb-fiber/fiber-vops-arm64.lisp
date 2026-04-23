;;;; VOP replacement for fiber_swap_context (arm64).
;;;;
;;;; Counterpart to fiber-vops-x86-64.lisp; see that file's header
;;;; comment for the overall shape.  AArch64 does not save return
;;;; addresses on the stack, so instead of pushing a RESUME label
;;;; below SP we compute its PC-relative address (ADR) and store it
;;;; directly into the ctx.lr slot that the struct fiber_context
;;;; layout reserves for x30.  The RET at the end of the VOP then
;;;; consumes the incoming fiber's ctx.lr -- either the resume label
;;;; of an earlier suspension or, for a new fiber,
;;;; fiber_trampoline_asm (seeded by sb_fiber_prepare).
;;;;
;;;; struct fiber_context layout (see fiber-arm64.h):
;;;;   0x00 sp      0x50 x26 (null)
;;;;   0x08 fp x29  0x58 x27 (csp)
;;;;   0x10 lr x30  0x60 x28 (cardtable)
;;;;   0x18 x19 (r9)     0x68 d8   0x88 d12
;;;;   0x20 x20 (r10)    0x70 d9   0x90 d13
;;;;   0x28 x21 (thread) 0x78 d10  0x98 d14
;;;;   0x30 x22 (lexenv) 0x80 d11  0xa0 d15
;;;;   0x38 x23 (nargs)
;;;;   0x40 x24 (nfp)
;;;;   0x48 x25 (ocfp)

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
  ;; Pin the ctx pointers to caller-saved regs (x0/x1).  The load
  ;; sequence below rewrites every AAPCS64 callee-saved GPR from
  ;; to->ctx; if either pointer lived in one of those, a subsequent
  ;; load would clobber it mid-sequence.
  (:temporary (:sc sap-reg :offset nl0-offset
               :from (:argument 0) :to (:result 0))
              from-tmp)
  (:temporary (:sc sap-reg :offset nl1-offset
               :from (:argument 1) :to (:result 0))
              to-tmp)
  ;; Scratch for SP<->reg moves and the PA-exit flag (x2, caller-saved).
  (:temporary (:sc unsigned-reg :offset nl2-offset) temp)
  (:generator 10
    (move from-tmp from)
    (move to-tmp   to)
    (let ((x19 (make-random-tn (sc-or-lose 'unsigned-reg) r9-offset))
          (x20 (make-random-tn (sc-or-lose 'unsigned-reg) r10-offset))
          ;; nfp has no predefined TN global (unlike nargs/ocfp/...),
          ;; so build one here.
          (nfp (make-random-tn (sc-or-lose 'any-reg) nfp-offset))
          (d8  (make-random-tn (sc-or-lose 'double-reg) 8))
          (d9  (make-random-tn (sc-or-lose 'double-reg) 9))
          (d10 (make-random-tn (sc-or-lose 'double-reg) 10))
          (d11 (make-random-tn (sc-or-lose 'double-reg) 11))
          (d12 (make-random-tn (sc-or-lose 'double-reg) 12))
          (d13 (make-random-tn (sc-or-lose 'double-reg) 13))
          (d14 (make-random-tn (sc-or-lose 'double-reg) 14))
          (d15 (make-random-tn (sc-or-lose 'double-reg) 15)))
      ;; SP can't appear as source/dest of STR (register 31 decodes to
      ;; XZR in that position); bounce it through TEMP.
      (inst mov-sp temp nsp-tn)
      (inst str temp (@ from-tmp 0))
      ;; Manufacture ctx.lr = RESUME.  Clobbering x30 here is fine:
      ;; any live caller return address has already been spilled to
      ;; this function's frame by the standard arm64 prologue; the
      ;; epilogue reloads it from the frame, not from x30 directly.
      (inst adr lr-tn RESUME)
      ;; Save callee-saved GPRs.  fp/lr first because that's where
      ;; the manufactured RESUME address is now waiting.
      (inst stp cfp-tn    lr-tn        (@ from-tmp #x08))
      (inst stp x19       x20          (@ from-tmp #x18))
      (inst stp thread-tn lexenv-tn    (@ from-tmp #x28))
      (inst stp nargs-tn  nfp          (@ from-tmp #x38))
      (inst stp ocfp-tn   null-tn      (@ from-tmp #x48))
      (inst stp csp-tn    cardtable-tn (@ from-tmp #x58))
      ;; Save callee-saved FP regs d8-d15.
      (inst stp d8  d9  (@ from-tmp #x68))
      (inst stp d10 d11 (@ from-tmp #x78))
      (inst stp d12 d13 (@ from-tmp #x88))
      (inst stp d14 d15 (@ from-tmp #x98))
      ;; Load the incoming context.  Leave SP for last so from-tmp /
      ;; to-tmp (in x0/x1, not saved here) stay valid throughout.
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
    ;; RET branches to x30 (= ctx.lr just loaded): a resumed fiber's
    ;; RESUME label, or fiber_trampoline_asm for a new fiber.
    (inst ret)
    ;; Resume landing pad -- reached when some other fiber's
    ;; %fiber-register-swap loads our saved sp/lr and RETs here.
    ;; Exit the pseudo-atomic region entered by our counterpart's
    ;; sb_fiber_switch_prep: clear PA_IN (low 32 bits), then check
    ;; PA_INTERRUPTED (high 32 bits) and trap if a signal was deferred.
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
