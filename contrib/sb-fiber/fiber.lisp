;;;; -*-  Lisp -*-
;;;;
;;;; sb-fiber Lisp shim.  Wraps fiber.c and x86-64-fiber.S.
;;;; x86-64 POSIX only.  See README.md for design notes.

(in-package :sb-fiber)

;;; Body is gated on :sb-fiber (so non-fiber builds compile to an empty
;;; fasl; the C runtime symbols this shim binds to are only linked when
;;; :sb-fiber is set) AND on x86-64 POSIX (the shim's only supported
;;; target).

#+(and sb-fiber x86-64 (not win32))
(progn

;;; --- Alien declarations for C runtime functions ---

(define-alien-routine ("sb_fiber_create" %fiber-create)
    system-area-pointer
  (stack-size unsigned-long)
  (binding-stack-size unsigned-long))

(define-alien-routine ("sb_fiber_create_main" %fiber-create-main)
    system-area-pointer
  (thread system-area-pointer))

(define-alien-routine ("sb_fiber_destroy" %fiber-destroy)
    void
  (fiber system-area-pointer))

(define-alien-routine ("sb_fiber_register" %fiber-register)
    void
  (thread system-area-pointer)
  (fiber system-area-pointer))

(define-alien-routine ("sb_fiber_unregister" %fiber-unregister)
    void
  (thread system-area-pointer)
  (fiber system-area-pointer))

(define-alien-routine ("sb_fiber_prepare" %fiber-prepare)
    void
  (fiber system-area-pointer)
  (fn system-area-pointer)
  (arg system-area-pointer))

;;; ALIEN-FUNCALL-SAVES-FP-AND-PC must be 0 here.  The default wrap
;;; binds *SAVED-FP* across the alien call, but prep swaps BSP from
;;; FROM's stack to TO's; a post-call UNBIND of *SAVED-FP* would pop
;;; a binding off TO's stack instead.  Prep itself does no stack
;;; walking, so suppressing the wrap is safe.
(declaim (inline %fiber-switch-prep))

(defun %fiber-switch-prep (from to)
  (declare (type system-area-pointer from to))
  (locally (declare (optimize (sb-c:alien-funcall-saves-fp-and-pc 0)))
    (alien-funcall (sb-alien:extern-alien "sb_fiber_switch_prep"
                    (function void system-area-pointer system-area-pointer))
                   from to))
  (values))

;;; %fiber-register-swap is translated by the VOP in
;;; x86-64-vops.lisp via a DEFKNOWN ALWAYS-TRANSLATABLE +
;;; :TRANSLATE clause.  This defun is only the uncompiled-call
;;; fallback; not declaimed inline, since inlining would shadow the
;;; VOP translation.
(defun %fiber-register-swap (from to)
  (declare (type system-area-pointer from to)
           (ignore from to))
  (error "sb-fiber: %fiber-register-swap called uncompiled."))

;;; --- Struct sb_fiber field offsets ---
;;; Queried from C at load time so the layout isn't duplicated here.
;;; LOAD-TIME-VALUE folds each offset into a constant displacement.

(define-alien-routine ("sb_fiber_struct_offset" %fiber-struct-offset)
    int
  (field int))

;;; Indices must match enum sb_fiber_field in fiber.c.
(defconstant +sb-fiber-field-state+         0)
(defconstant +sb-fiber-field-owner+         1)
;; +sb-fiber-field-bsp+ (2) is read only by C; the gap keeps numbering aligned.
(defconstant +sb-fiber-field-catch+         3)
(defconstant +sb-fiber-field-unwind+        4)
(defconstant +sb-fiber-field-return-fiber+  5)

;;; State values; must match enum fiber_state in fiber.h.
(defconstant +fiber-new+      0)
(defconstant +fiber-runnable+ 1)
(defconstant +fiber-running+  2)
(defconstant +fiber-dead+     3)

(defstruct (fiber (:constructor %make-fiber))
  (sap (sb-sys:int-sap 0) :type sb-sys:system-area-pointer)
  (function nil :type (or null function))
  ;; Set by SB-FIBER-LISP-ENTRY if the entry function escapes via an
  ;; unhandled condition; FIBER-SWITCH re-signals it in the caller's
  ;; context on the next switch out, so the condition never unwinds
  ;; across the fiber boundary into a different stack.
  (pending-condition nil :type (or null condition))
  ;; Nesting depth of WITH-FIBER-PINNED.  FIBER-SWITCH refuses a
  ;; nonzero FROM.  See WITH-FIBER-PINNED's docstring for the rationale.
  (pin-count 0 :type (and fixnum unsigned-byte))
  ;; Lisp-side mirror of the C return_fiber slot, kept here so that
  ;; FIBER-YIELD doesn't have to reverse-look-up a SAP via the registry
  ;; (the main fiber is deliberately not in *fiber-registry*).
  (return-fiber nil :type (or null fiber)))

;;; *current-fiber* is per-thread.  We force a wired TLS slot via
;;; :always-thread-local so SETF on the unbound special goes to TLS,
;;; not to the shared global value.  Without this, two threads that
;;; both call MAKE-MAIN-FIBER end up clobbering each other's notion
;;; of "current fiber" through the global symbol-value cell.
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sb-thread
  (setf (sb-int:info :variable :wired-tls '*current-fiber*)
        :always-thread-local))

(defvar *current-fiber* nil
  "Fiber currently running on this thread, or NIL.")

(declaim (inline current-fiber))
(defun current-fiber () *current-fiber*)

;;; Trampoline invoked by fiber_trampoline_c via the C entry_fn slot.
;;; Re-binds *HANDLER-CLUSTERS* and *RESTART-CLUSTERS* to isolate the
;;; fiber from handlers established on the caller's stack -- those
;;; handlers' RETURN-FROM exit points reference frames on a different
;;; stack and unwinding to them would corrupt SB-C::UNWIND.  Anything
;;; that escapes the user function is captured on the fiber for
;;; FIBER-SWITCH to re-signal.
(define-alien-callable sb-fiber-lisp-entry void
    ((arg unsigned-long))
  ;; ARG is the C sb_fiber* SAP-as-integer (a stable malloc address);
  ;; we look up the Lisp wrapper via *fiber-registry*.
  (let ((f (%fiber-registry-get arg)))
    (when (and f (fiber-function f))
      (let ((sb-kernel:*handler-clusters* sb-kernel::**initial-handler-clusters**)
            (sb-kernel:*restart-clusters* nil))
        (handler-case
            (funcall (fiber-function f))
          (condition (c)
            (setf (fiber-pending-condition f) c)))))))

;;; Computed once at load time -- the alien-callable address is stable
;;; across the image's life.  Doing this at top-level avoids a
;;; first-use race where two threads could each create a callable.
(defvar *lisp-entry-sap*
  (sb-alien:alien-sap (alien-callable-function 'sb-fiber-lisp-entry)))

(declaim (inline lisp-entry-sap))
(defun lisp-entry-sap () *lisp-entry-sap*)

;;; Registry: stable C SAP (integer) -> Lisp wrapper.  We can't put
;;; the Lisp wrapper's address in the C entry_arg slot, because a
;;; copying GC could move the wrapper while the C side still held the
;;; old address.  The C SAP doesn't move, so we key on its int value.
(defvar *fiber-registry-lock*
  (sb-thread:make-mutex :name "sb-fiber registry"))
(defvar *fiber-registry* (make-hash-table :test 'eql))

(defun %fiber-registry-put (fiber)
  (sb-thread:with-mutex (*fiber-registry-lock*)
    (setf (gethash (sb-sys:sap-int (fiber-sap fiber)) *fiber-registry*)
          fiber)))

(defun %fiber-registry-get (sap-int)
  (sb-thread:with-mutex (*fiber-registry-lock*)
    (gethash sap-int *fiber-registry*)))

(defun %fiber-registry-remove (fiber)
  (sb-thread:with-mutex (*fiber-registry-lock*)
    (remhash (sb-sys:sap-int (fiber-sap fiber)) *fiber-registry*)))

;;; --- Field accessors ---
;;;
;;; INLINE defuns with LOAD-TIME-VALUE offsets so the offset folds
;;; into a constant displacement at every call site.

(declaim (inline %fiber-state-at %fiber-owner-at
                 %fiber-catch-at %fiber-unwind-at
                 %fiber-return-fiber-at
                 (setf %fiber-state-at) (setf %fiber-catch-at)
                 (setf %fiber-unwind-at) (setf %fiber-return-fiber-at)))

(defun %fiber-state-at (sap)
  (sb-sys:signed-sap-ref-32
   sap (load-time-value (%fiber-struct-offset +sb-fiber-field-state+) t)))

(defun (setf %fiber-state-at) (new sap)
  (setf (sb-sys:signed-sap-ref-32
         sap (load-time-value (%fiber-struct-offset +sb-fiber-field-state+) t))
        new))

(defun %fiber-owner-at (sap)
  (sb-sys:sap-ref-sap
   sap (load-time-value (%fiber-struct-offset +sb-fiber-field-owner+) t)))

(defun %fiber-catch-at (sap)
  (sb-sys:sap-ref-word
   sap (load-time-value (%fiber-struct-offset +sb-fiber-field-catch+) t)))

(defun (setf %fiber-catch-at) (new sap)
  (setf (sb-sys:sap-ref-word
         sap (load-time-value (%fiber-struct-offset +sb-fiber-field-catch+) t))
        new))

(defun %fiber-unwind-at (sap)
  (sb-sys:sap-ref-word
   sap (load-time-value (%fiber-struct-offset +sb-fiber-field-unwind+) t)))

(defun (setf %fiber-unwind-at) (new sap)
  (setf (sb-sys:sap-ref-word
         sap (load-time-value (%fiber-struct-offset +sb-fiber-field-unwind+) t))
        new))

(defun (setf %fiber-return-fiber-at) (new sap)
  (setf (sb-sys:sap-ref-sap
         sap
         (load-time-value (%fiber-struct-offset +sb-fiber-field-return-fiber+) t))
        new))

;; Thread-struct field access via existing VM slot constants.
(declaim (inline %thread-catch %thread-unwind
                 (setf %thread-catch) (setf %thread-unwind)))

(defun %thread-catch (sap)
  (sb-sys:sap-ref-word
   sap (ash sb-vm::thread-current-catch-block-slot sb-vm:word-shift)))

(defun (setf %thread-catch) (new sap)
  (setf (sb-sys:sap-ref-word
         sap (ash sb-vm::thread-current-catch-block-slot sb-vm:word-shift))
        new))

(defun %thread-unwind (sap)
  (sb-sys:sap-ref-word
   sap (ash sb-vm::thread-current-unwind-protect-block-slot sb-vm:word-shift)))

(defun (setf %thread-unwind) (new sap)
  (setf (sb-sys:sap-ref-word
         sap (ash sb-vm::thread-current-unwind-protect-block-slot sb-vm:word-shift))
        new))

;;; --- Lifecycle ---

(defun make-main-fiber ()
  "Create a fiber representing the current thread's own stack.
This is used as the FROM argument to FIBER-SWITCH when switching
from the main thread context to a fiber. Must be destroyed when
no longer needed (but not while it is running).
Binds *CURRENT-FIBER* on the calling thread."
  (let ((sap (%fiber-create-main (sb-thread:current-thread-sap))))
    (when (sb-sys:sap= sap (sb-sys:int-sap 0))
      (error "Failed to allocate main fiber"))
    (let ((f (%make-fiber :sap sap :function nil)))
      (%fiber-register (sb-thread:current-thread-sap) sap)
      (setf *current-fiber* f)
      f)))

(defun make-fiber (function &key (stack-size 65536)
                                  (binding-stack-size 8192))
  "Create a fiber that will execute FUNCTION (a zero-argument function)
when first switched to. The fiber must be destroyed with DESTROY-FIBER
when no longer needed.

When FUNCTION returns, the fiber is automatically marked DEAD and
control switches back to the fiber that most recently switched to it."
  (let ((sap (%fiber-create stack-size binding-stack-size)))
    (when (sb-sys:sap= sap (sb-sys:int-sap 0))
      (error "Failed to allocate fiber"))
    (let ((f (%make-fiber :sap sap :function function)))
      ;; Prepare BEFORE register: registration publishes the fiber to
      ;; fiber_list where the GC will conservatively scan
      ;; [ctx.rsp .. stack_end); ctx.rsp is 0 until prepare writes
      ;; the trampoline frame, so reversing the order opens a window
      ;; for a concurrent GC to dereference the null page.
      ;;
      ;; The entry-arg is the C SAP, not the Lisp wrapper -- see the
      ;; registry comment.
      (%fiber-prepare sap (lisp-entry-sap) sap)
      (%fiber-register (sb-thread:current-thread-sap) sap)
      (%fiber-registry-put f)
      f)))

(defun destroy-fiber (fiber)
  "Deallocate FIBER's stacks. FIBER must not be currently running.
For main fibers, call this only after you've finished all fiber switching."
  (unless (sb-sys:sap= (fiber-sap fiber) (sb-sys:int-sap 0))
    (when (= (%fiber-state-at (fiber-sap fiber)) +fiber-running+)
      (setf (%fiber-state-at (fiber-sap fiber)) +fiber-runnable+))
    (%fiber-registry-remove fiber)
    (%fiber-destroy (fiber-sap fiber))
    (setf (fiber-sap fiber) (sb-sys:int-sap 0)
          (fiber-function fiber) nil))
  (values))

(defun fiber-alive-p (fiber)
  "Return T if FIBER has not yet finished or been destroyed."
  (not (sb-sys:sap= (fiber-sap fiber) (sb-sys:int-sap 0))))

(defun fiber-state (fiber)
  "Return the current state of FIBER as an integer.
0=new, 1=runnable, 2=running, 3=dead."
  (if (fiber-alive-p fiber)
      (%fiber-state-at (fiber-sap fiber))
      +fiber-dead+))

;;; Context switch.  This shim owns validation, catch/unwind
;;; save+pre-install, return_fiber, and *CURRENT-FIBER*.  C owns BSP,
;;; the Lisp-stack-bounds swap, state transitions, and the binding-
;;; stack swap (all inside the PA region established by prep); the
;;; %FIBER-REGISTER-SWAP VOP performs the register/SP swap inline.
;;; See README.md "Pseudo-atomic switch window".

(defun fiber-switch (from to)
  "Suspend FROM and resume TO. Both must be registered to the current thread.
FROM must be RUNNING; TO must be RUNNABLE or NEW.
Returns when another fiber switches back to FROM (or when TO's entry
function returns, which auto-switches back to FROM).

Signals an error if the switch is invalid (wrong states or wrong thread).
If TO's entry function escaped via an unhandled condition that was
captured by SB-FIBER-LISP-ENTRY, that condition is re-signalled here,
in the caller's context."
  (declare (type fiber from to))
  (let ((from-sap (fiber-sap from))
        (to-sap   (fiber-sap to)))
    (when (or (sb-sys:sap= from-sap (sb-sys:int-sap 0))
              (sb-sys:sap= to-sap   (sb-sys:int-sap 0)))
      (error "fiber-switch: destroyed fiber"))
    (let ((th-sap (sb-thread:current-thread-sap)))
      (unless (sb-sys:sap= (%fiber-owner-at from-sap) th-sap)
        (error "fiber-switch: FROM fiber is owned by a different thread"))
      (unless (sb-sys:sap= (%fiber-owner-at to-sap) th-sap)
        (error "fiber-switch: TO fiber is owned by a different thread"))
      (unless (= (%fiber-state-at from-sap) +fiber-running+)
        (error "fiber-switch: FROM fiber is not RUNNING"))
      (unless (zerop (fiber-pin-count from))
        (error "fiber-switch: FROM fiber is pinned (depth ~D)"
               (fiber-pin-count from)))
      (let ((ts (%fiber-state-at to-sap)))
        (unless (or (= ts +fiber-runnable+) (= ts +fiber-new+))
          (error "fiber-switch: TO fiber is not RUNNABLE or NEW")))
      ;; Record return path: C SAP (fiber_trampoline_c auto-return) and
      ;; Lisp wrapper (FIBER-YIELD).
      (setf (%fiber-return-fiber-at to-sap) from-sap
            (fiber-return-fiber to)        from)
      ;; Save the outgoing catch/unwind chains and pre-install TO's.
      (setf (%fiber-catch-at  from-sap) (%thread-catch  th-sap)
            (%fiber-unwind-at from-sap) (%thread-unwind th-sap))
      (setf (%thread-catch  th-sap) (%fiber-catch-at  to-sap)
            (%thread-unwind th-sap) (%fiber-unwind-at to-sap))
      (setf *current-fiber* to)
      ;; PREP enters PA and stages BSP / Lisp-stack bounds / state /
      ;; binding-stack swap.  The VOP performs register/SP swap and
      ;; emits exit-PA at its RESUME tail.
      (%fiber-switch-prep from-sap to-sap)
      (%fiber-register-swap from-sap to-sap)
      ;; Resumed here as FROM.
      (setf *current-fiber* from)))
  (let ((c (fiber-pending-condition to)))
    (when c
      (setf (fiber-pending-condition to) nil)
      (error c)))
  (values))

(defmacro with-fiber ((var function &rest make-args) &body body)
  "Create a fiber, bind it to VAR, execute BODY, destroy on exit."
  `(let ((,var (make-fiber ,function ,@make-args)))
     (unwind-protect (progn ,@body)
       (destroy-fiber ,var))))

(defun fiber-yield ()
  "Switch from the current fiber to its return fiber (the one that
most recently switched to it).  Equivalent to
  (fiber-switch *current-fiber* (fiber-return-fiber *current-fiber*))
Errors if there is no current fiber or no recorded return fiber."
  (let ((self *current-fiber*))
    (unless self
      (error "fiber-yield: no current fiber"))
    (let ((target (fiber-return-fiber self)))
      (unless target
        (error "fiber-yield: ~S has no return fiber (never resumed)"
               self))
      (fiber-switch self target))))

(declaim (inline fiber-pinned-p))
(defun fiber-pinned-p (fiber)
  "Return T if FIBER's pin count is nonzero (FIBER-SWITCH refuses to
suspend such a fiber)."
  (declare (type fiber fiber))
  (plusp (fiber-pin-count fiber)))

(defmacro with-fiber-pinned (() &body body)
  "Increment the current fiber's pin count for the dynamic extent of
BODY; FIBER-SWITCH refuses to suspend a pinned fiber.  Pins nest.
Errors at entry if there is no current fiber.  Intended for FFI; see
the manual for the rationale."
  (let ((f (gensym "PINNED-FIBER")))
    `(let ((,f (or *current-fiber*
                   (error "with-fiber-pinned: no current fiber"))))
       (incf (fiber-pin-count ,f))
       (unwind-protect (progn ,@body)
         (decf (fiber-pin-count ,f))))))

) ; #+(and sb-fiber x86-64 (not win32)) (progn ...
