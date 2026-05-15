;;;; -*-  Lisp -*-
;;;;
;;;; sb-fiber API

(in-package :sb-fiber)

#+sb-fiber
(progn

;;; --- Lifecycle ---

(defun make-main-fiber (&key name)
  "Create a fiber representing the current thread's own stack and bind
*CURRENT-FIBER* to it.  Most users should call WITH-FIBER-THREAD
instead, or just call MAKE-FIBER, which creates a main fiber lazily.
NAME, if supplied, is a string label used by PRINT-OBJECT."
  (let ((sap (%fiber-create-main (sb-thread:current-thread-sap))))
    (when (sb-sys:sap= sap (sb-sys:int-sap 0))
      (error "Failed to allocate main fiber"))
    (let ((f (%make-fiber :sap sap :function nil :name name)))
      (%fiber-register (sb-thread:current-thread-sap) sap)
      (setf *current-fiber* f)
      f)))

(defmacro with-fiber-thread (() &body body)
  "Register a main fiber on the calling thread for the dynamic extent
of BODY and release it on exit.  Equivalent to MAKE-MAIN-FIBER +
UNWIND-PROTECT + RELEASE-FIBER, plus a no-op if BODY is reached with
*CURRENT-FIBER* already bound (e.g. nested WITH-FIBER-THREAD)."
  (let ((self (gensym "MAIN-FIBER"))
        (existing (gensym "EXISTING-MAIN")))
    `(let* ((,existing *current-fiber*)
            (,self (or ,existing (make-main-fiber))))
       (declare (ignorable ,self))
       (unwind-protect (progn ,@body)
         (unless ,existing (release-fiber ,self))))))

(defun make-fiber (function &key name
                                 (stack-size 65536)
                                 (binding-stack-size 8192))
  "Create a fiber that runs FUNCTION (zero-argument) when first
switched to.  NAME is a string label used by PRINT-OBJECT.  Implicitly
ensures a main fiber for the calling thread.

When FUNCTION returns, the fiber is marked DEAD and control switches
back to its most recent resumer, delivering FUNCTION's return value
as that resumer's SWITCH-FIBER value.

If the calling thread has no main fiber yet, creates one (binding
*CURRENT-FIBER*) so the new fiber has a switch-back target.

Fibers are auto-released when their owning thread exits; explicit
RELEASE-FIBER is only needed if you want to reclaim resources sooner."
  (unless *current-fiber* (make-main-fiber))
  (let ((sap (%fiber-create stack-size binding-stack-size)))
    (when (sb-sys:sap= sap (sb-sys:int-sap 0))
      (error "Failed to allocate fiber"))
    (let ((f (%make-fiber :sap sap :function function :name name)))
      (%fiber-prepare sap
                      (sb-alien:alien-sap
                       (alien-callable-function 'sb-fiber-lisp-entry))
                      sap)
      (%fiber-register (sb-thread:current-thread-sap) sap)
      f)))

(defun release-fiber (fiber)
  "Release FIBER.  For a worker fiber, unmaps its stacks and frees the
bookkeeping struct.  For a main fiber, just frees the struct; the
thread's own stack and binding stack are untouched.  FIBER must not be
RUNNING (other than a main fiber, which is always RUNNING until
released).  Optional: registered fibers are released automatically
when their owning thread exits."
  (unless (sb-sys:sap= (fiber-sap fiber) (sb-sys:int-sap 0))
    (let* ((sap (fiber-sap fiber))
           (c   (fiber-c sap)))
      ;; A main fiber represents the calling thread's own stack and is
      ;; perpetually RUNNING; demote it so the underlying state machine
      ;; agrees we're tearing it down.
      (when (= (sb-fiber-c-state c) +fiber-running+)
        (when (fiber-function fiber)
          (error "release-fiber: ~S is RUNNING" fiber))
        (setf (sb-fiber-c-state c) +fiber-runnable+))
      (let ((owner (sb-fiber-c-owner c)))
        (when (and (not (zerop owner))
                   (= owner (sb-sys:sap-int (sb-thread:current-thread-sap))))
          (%fiber-unregister (sb-thread:current-thread-sap) sap)))
      (%fiber-destroy sap)
      (setf (fiber-sap fiber) (sb-sys:int-sap 0)
            (fiber-function fiber) nil
            (fiber-return-fiber fiber) nil
            (fiber-pending-condition fiber) nil)
      (when (eq *current-fiber* fiber)
        (setf *current-fiber* nil)))))

(defun fiber-state (fiber)
  "Return the current state of FIBER as an integer.
0=new, 1=runnable, 2=running, 3=dead."
  (if (sb-sys:sap= (fiber-sap fiber) (sb-sys:int-sap 0))
      +fiber-dead+
      (sb-fiber-c-state (fiber-c (fiber-sap fiber)))))

(defun fiber-alive-p (fiber)
  "Return T if FIBER has not finished running and has not been
released."
  (/= (fiber-state fiber) +fiber-dead+))

(defun switch-fiber (from to &optional value)
  "Suspend FROM and resume TO, delivering VALUE.  Returns whatever
the next switch back to FROM delivers, or TO's entry-function return
value if TO ran to completion.

Both fibers must be on the current thread; FROM must be RUNNING and
TO RUNNABLE or NEW.  A condition that escaped TO's entry function is
re-signalled here."
  (declare (type fiber from to))
  (let ((from-sap (fiber-sap from))
        (to-sap   (fiber-sap to)))
    (when (or (sb-sys:sap= from-sap (sb-sys:int-sap 0))
              (sb-sys:sap= to-sap   (sb-sys:int-sap 0)))
      (error "switch-fiber: released fiber"))
    (let ((th-sap (sb-thread:current-thread-sap))
          (from-c (fiber-c from-sap))
          (to-c   (fiber-c to-sap)))
      (let ((th-int (sb-sys:sap-int th-sap)))
        (unless (= (sb-fiber-c-owner from-c) th-int)
          (error "switch-fiber: FROM fiber is owned by a different thread"))
        (unless (= (sb-fiber-c-owner to-c) th-int)
          (error "switch-fiber: TO fiber is owned by a different thread")))
      (unless (= (sb-fiber-c-state from-c) +fiber-running+)
        (error "switch-fiber: FROM fiber is not RUNNING"))
      (unless (zerop (fiber-pin-count from))
        (error "switch-fiber: FROM fiber is pinned (depth ~D)"
               (fiber-pin-count from)))
      (let ((ts (sb-fiber-c-state to-c)))
        (unless (or (= ts +fiber-runnable+) (= ts +fiber-new+))
          (error "switch-fiber: TO fiber is not RUNNABLE or NEW")))
      ;; Stage return path and value.  Safe to do outside PA: these
      ;; touch Lisp-owned slots only.
      (setf (sb-fiber-c-return-fiber to-c) (sb-sys:sap-int from-sap)
            (fiber-return-fiber to)        from
            (fiber-value to)               value
            *current-fiber*                to)
      (%switch-fiber-prep from-sap to-sap)
      ;; Catch/unwind chains point into the running fiber's stack
      ;; so they swap atomically with SP, inside PA.
      (let ((tc (%thread-slot th-sap sb-vm::thread-current-catch-block-slot))
            (tu (%thread-slot th-sap sb-vm::thread-current-unwind-protect-block-slot)))
        (setf (sb-fiber-c-catch from-c)  tc
              (sb-fiber-c-unwind from-c) tu
              (%thread-slot th-sap sb-vm::thread-current-catch-block-slot)
                (sb-fiber-c-catch to-c)
              (%thread-slot th-sap sb-vm::thread-current-unwind-protect-block-slot)
                (sb-fiber-c-unwind to-c)))
      ;; BSP install (prep already captured FROM's BSP); also
      ;; thread.binding_stack_start.
      (setf (%thread-slot th-sap sb-vm::thread-binding-stack-pointer-slot)
              (sb-fiber-c-bsp to-c)
            (%thread-slot th-sap sb-vm::thread-binding-stack-start-slot)
              (sb-fiber-c-bs-start to-c))
      ;; State flip.  Auto-return calls %SWITCH-FIBER-PREP with FROM
      ;; already DEAD; preserve it.
      (when (= (sb-fiber-c-state from-c) +fiber-running+)
        (setf (sb-fiber-c-state from-c) +fiber-runnable+))
      (setf (sb-fiber-c-state to-c) +fiber-running+)
      (%fiber-register-swap from-sap to-sap)
      (setf *current-fiber* from)))
  (let ((c (fiber-pending-condition from)))
    (when c
      (setf (fiber-pending-condition from) nil)
      (error c)))
  (let ((c (fiber-pending-condition to)))
    (when c
      (setf (fiber-pending-condition to) nil)
      (error c)))
  (fiber-value from))

(defmacro with-fiber ((var function &rest make-args) &body body)
  "Create a fiber, bind it to VAR, execute BODY, release on exit."
  `(let ((,var (make-fiber ,function ,@make-args)))
     (unwind-protect (progn ,@body)
       (release-fiber ,var))))

(defun yield-fiber (&optional value)
  "Switch from the current fiber to its return fiber, delivering VALUE.
Returns the value supplied by the next switch-in.  Errors if the
current fiber has no recorded return fiber (never resumed)."
  (let ((self *current-fiber*))
    (let ((target (fiber-return-fiber self)))
      (unless target
        (error "yield-fiber: ~S has no return fiber (never resumed)"
               self))
      (switch-fiber self target value))))

(defun join-fiber (fiber)
  "Resume FIBER from the current fiber until it runs to completion,
and return its entry-function value.  FIBER must be RUNNABLE or NEW
and on the current thread."
  (declare (type fiber fiber))
  (loop until (= (fiber-state fiber) +fiber-dead+)
        for v = (switch-fiber *current-fiber* fiber)
        finally (return v)))

(defun interrupt-fiber (fiber condition)
  "Stage CONDITION to be signalled in FIBER on its next resume.
Cooperative -- nothing happens until SWITCH-FIBER delivers control
into FIBER, at which point CONDITION is signalled in the fiber's own
context.  A NEW fiber is effectively cancelled (the condition fires
before its entry function runs)."
  (declare (type fiber fiber)
           (type condition condition))
  (setf (fiber-pending-condition fiber) condition)
  fiber)

(declaim (inline fiber-condition))
(defun fiber-condition (fiber)
  "The condition staged for FIBER's next resume, or NIL.  Set by
INTERRUPT-FIBER, or by an entry function escaping via an unhandled
condition; cleared when consumed."
  (declare (type fiber fiber))
  (fiber-pending-condition fiber))

(defun make-fiber-generator (function &key name)
  "Wrap FUNCTION in a fiber and return a thunk that, on each call,
returns the fiber's next YIELD-FIBER value (or its entry-function's
return value).  Returns NIL once the fiber finishes, and releases the
fiber."
  (let ((f (make-fiber function :name name)))
    (lambda ()
      (cond ((not (fiber-alive-p f)) nil)
            ((= (fiber-state f) +fiber-dead+)
             (release-fiber f)
             nil)
            (t (switch-fiber *current-fiber* f))))))

(defmacro do-fiber-generator ((var generator) &body body)
  "Iterate VAR over the values produced by GENERATOR (a thunk built
by MAKE-FIBER-GENERATOR), terminating when it returns NIL."
  (let ((gen (gensym "GEN")))
    `(let ((,gen ,generator))
       (loop for ,var = (funcall ,gen)
             while ,var do ,@body))))

(declaim (inline fiber-pinned-p))
(defun fiber-pinned-p (fiber)
  "Return T if FIBER's pin count is greater than zero. SWITCH-FIBER
will only suspend a fiber with a pin count of zero."
  (declare (type fiber fiber))
  (plusp (fiber-pin-count fiber)))

(defmacro with-fiber-pinned (() &body body)
  "Increment the current fiber's pin count for the dynamic extent of
BODY; SWITCH-FIBER refuses to suspend a pinned fiber.  Pins nest."
  (let ((f (gensym "PINNED-FIBER")))
    `(let ((,f (or *current-fiber*
                   (error "with-fiber-pinned: no current fiber"))))
       (incf (fiber-pin-count ,f))
       (unwind-protect (progn ,@body)
         (decf (fiber-pin-count ,f))))))

)
