;;;; VOP replacement for fiber_swap_context (x86-64).
;;;;
;;;; Emits inline the register/SP swap that used to live in
;;;; src/runtime/fiber_switch_amd64.S, avoiding the alien-call
;;;; + INVOKE-WITH-SAVED-FP + C-stub overhead of reaching the asm
;;;; through an alien-funcall.
;;;;
;;;; The classic fiber_swap_context relied on the CALL instruction
;;;; pushing its return address onto RSP, so that saving RSP captured
;;;; a continuation-address-on-top pattern and the final RET popped
;;;; that address into RIP on resumption.  A VOP is inlined -- there
;;;; is no CALL into it -- so we manufacture the same pattern by
;;;; LEAing a RIP-relative address of a RESUME label into a scratch
;;;; register and PUSHing it as the "return address" before we save
;;;; RSP.  When someone switches back to us, their register swap
;;;; loads our saved RSP (pointing at that RESUME label we pushed)
;;;; and RETs, popping the label into RIP and landing us back at
;;;; RESUME -- immediately after the VOP in the caller's code.

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
  ;; Pin temps to caller-saved registers.  The load sequence below
  ;; writes every callee-saved register (RBX, RBP, R12-R15) from
  ;; to->ctx; if to-tmp lived in one of those, a later store would
  ;; clobber the pointer mid-sequence.  Using RAX/RCX/RDX guarantees
  ;; independence.
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
    ;; Manufacture the CALL/RET pair: push a RIP-relative address of
    ;; RESUME as the "saved return address".  The subsequent save of
    ;; RSP therefore captures an RSP pointing at [RESUME-addr], which
    ;; is exactly what a future RET (from whoever resumes us) needs.
    (inst lea temp (rip-relative-ea RESUME))
    (inst push temp)
    ;; Save callee-saved registers + RSP into from->ctx.  Offsets
    ;; match struct fiber_context in fiber-x86-64.h:
    ;;   rsp=0 rbx=8 rbp=16 r12=24 r13=32 r14=40 r15=48
    (inst mov (ea 0  from-tmp) rsp-tn)
    (inst mov (ea 8  from-tmp) rbx-tn)
    (inst mov (ea 16 from-tmp) rbp-tn)
    (inst mov (ea 24 from-tmp) r12-tn)
    (inst mov (ea 32 from-tmp) r13-tn)
    (inst mov (ea 40 from-tmp) r14-tn)
    (inst mov (ea 48 from-tmp) r15-tn)
    ;; Load callee-saved registers from to->ctx, then RSP last (so
    ;; all the reads from [to-tmp + N] happen before we leave this
    ;; stack -- to-tmp aliases RCX, unaffected by the loads).
    (inst mov r15-tn (ea 48 to-tmp))
    (inst mov r14-tn (ea 40 to-tmp))
    (inst mov r13-tn (ea 32 to-tmp))
    (inst mov r12-tn (ea 24 to-tmp))
    (inst mov rbp-tn (ea 16 to-tmp))
    (inst mov rbx-tn (ea 8  to-tmp))
    (inst mov rsp-tn (ea 0  to-tmp))
    ;; RET pops the target's pushed RESUME address (or, for a new
    ;; fiber, the trampoline's seeded entry RIP) and jumps there.
    (inst ret)
    ;; Resume landing pad.  We arrive here when someone switches back
    ;; to us (their register swap loads our saved RSP, which pointed
    ;; at this label, and RETs).  At this point the thread's
    ;; pseudo_atomic_bits are still set -- sb_fiber_switch_prep set
    ;; them, and nobody has cleared them since -- so we exit PA here,
    ;; trapping to dispatch any interrupt deferred during the switch.
    RESUME
    (emit-end-pseudo-atomic)))
