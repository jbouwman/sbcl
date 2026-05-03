;;;; Inline register/SP swap for FIBER-SWITCH (x86-64).
;;;;
;;;; The asm fiber_swap_context relies on CALL pushing the return
;;;; address; an inlined VOP has no CALL, so we LEA RIP-relative the
;;;; RESUME label and PUSH it as the saved-RIP before saving RSP.
;;;; Whoever resumes us loads that RSP and RETs to RESUME.

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
  ;; Pin temps to caller-saved regs (RAX/RCX/RDX); the load sequence
  ;; below rewrites every callee-saved register, so to-tmp must not
  ;; alias one of them.
  (:temporary (:sc sap-reg :offset rax-offset
               :from (:argument 0) :to (:result 0))
              from-tmp)
  (:temporary (:sc sap-reg :offset rcx-offset
               :from (:argument 1) :to (:result 0))
              to-tmp)
  (:temporary (:sc unsigned-reg :offset rdx-offset) temp)
  (:generator 10
    (move from-tmp from)
    (move to-tmp   to)
    ;; Push RIP-relative RESUME as the saved-RIP, so the captured RSP
    ;; points at [RESUME-addr] for a future RET to consume.
    (inst lea temp (rip-relative-ea RESUME))
    (inst push temp)
    ;; Save callee-saved regs + RSP into from->ctx.  Offsets match
    ;; struct fiber_context in x86-64-fiber.h.
    (inst mov (ea 0  from-tmp) rsp-tn)
    (inst mov (ea 8  from-tmp) rbx-tn)
    (inst mov (ea 16 from-tmp) rbp-tn)
    (inst mov (ea 24 from-tmp) r12-tn)
    (inst mov (ea 32 from-tmp) r13-tn)
    (inst mov (ea 40 from-tmp) r14-tn)
    (inst mov (ea 48 from-tmp) r15-tn)
    ;; Load callee-saved regs from to->ctx; RSP last, so to-tmp (RCX)
    ;; stays valid while we still need it.
    (inst mov r15-tn (ea 48 to-tmp))
    (inst mov r14-tn (ea 40 to-tmp))
    (inst mov r13-tn (ea 32 to-tmp))
    (inst mov r12-tn (ea 24 to-tmp))
    (inst mov rbp-tn (ea 16 to-tmp))
    (inst mov rbx-tn (ea 8  to-tmp))
    (inst mov rsp-tn (ea 0  to-tmp))
    ;; RET pops the target's saved RIP (RESUME, or a new fiber's
    ;; trampoline entry seeded by sb_fiber_prepare).
    (inst ret)
    ;; Resume tail: PA is still set from prep -- exit it now and
    ;; trap if a signal was deferred during the switch.
    RESUME
    (emit-end-pseudo-atomic)))
