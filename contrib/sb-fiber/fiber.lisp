;;;; -*-  Lisp -*-
;;;;
;;;; Fiber (stackful coroutine) support for SBCL.
;;;; Wraps the C runtime fiber primitives (fiber.c and the per-arch
;;;; assembly in fiber_switch_<arch>.S).  Currently implemented on
;;;; x86-64 and arm64.

(in-package :sb-fiber)

;;; --- Platform guard ---
;;;
;;; The C runtime is wired into x86-64 and arm64 non-Windows Config
;;; files only, and sb-fiber's signal-handling and mmap tricks
;;; assume POSIX.  On unsupported platforms the reader skips the
;;; whole body below: compile-file never tries to intern arch-
;;; specific SB-VM symbols, and load succeeds silently so that
;;; (require :sb-fiber) during a cross-platform contrib build
;;; doesn't error.  The exported symbols remain interned (via
;;; package.lisp) but unbound, so any actual fiber call on an
;;; unsupported platform signals an informative undefined-function
;;; error rather than a cryptic alien-lookup failure.

#+(and (or x86-64 arm64) (not win32))
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

;;; The prep alien call is wrapped as an inline Lisp function that
;;; sets ALIEN-FUNCALL-SAVES-FP-AND-PC to 0, suppressing
;;; INVOKE-WITH-SAVED-FP.  That wrap is incompatible with prep here:
;;; prep swaps the thread's binding-stack-pointer from FROM's bs to
;;; TO's bs, so a post-call UNBIND of *SAVED-FP* would read and
;;; decrement TO's BSP -- popping a binding off TO's stack.  Skipping
;;; the wrap is safe because prep does no stack walking nor calls
;;; out to code that does.
;;;
;;; There is no %fiber-switch-finish: signal safety is achieved with
;;; pseudo-atomic rather than sigprocmask, and the exit-PA sequence
;;; is emitted inline in the %fiber-register-swap VOP's resume tail.
(declaim (inline %fiber-switch-prep))

(defun %fiber-switch-prep (from to)
  (declare (type system-area-pointer from to))
  (locally (declare (optimize (sb-c:alien-funcall-saves-fp-and-pc 0)))
    (alien-funcall (sb-alien:extern-alien "sb_fiber_switch_prep"
                    (function void system-area-pointer system-area-pointer))
                   from to))
  (values))

;;; %fiber-register-swap is a VOP (see fiber-vops-x86-64.lisp and
;;; fiber-vops-arm64.lisp); fiber.lisp is only reached in compiled
;;; form, so the compiler source-transforms calls through the
;;; DEFKNOWN ALWAYS-TRANSLATABLE declaration + the VOP's :TRANSLATE
;;; clause.  A defun-level function binding is provided only as a
;;; fallback for calls the compiler cannot translate (none in
;;; practice; raised so uncompiled calls yield a clean error rather
;;; than UNDEFINED-FUNCTION).  It is NOT declaimed inline: with
;;; inline on, the compiler prefers splicing this body in over the
;;; VOP translation, which is exactly the opposite of what we want.
(defun %fiber-register-swap (from to)
  (declare (type system-area-pointer from to)
           (ignore from to))
  (error "sb-fiber: %fiber-register-swap called uncompiled -- the ~
          VOP translation should have replaced this call."))

;;; --- Struct sb_fiber field offsets ---
;;;
;;; Queried from C at load time (see sb_fiber_struct_offset in fiber.c)
;;; so the per-arch struct layout is never hardcoded here.  Each offset
;;; accessor below uses LOAD-TIME-VALUE so the resulting byte offset
;;; becomes a constant inside the inlined caller.

(define-alien-routine ("sb_fiber_struct_offset" %fiber-struct-offset)
    int
  (field int))

;; Field-index constants; must match enum sb_fiber_field in fiber.c.
(defconstant +sb-fiber-field-state+         0)
(defconstant +sb-fiber-field-owner+         1)
;; BSP offset is accessed only from C (see sb_fiber_switch_prep),
;; not here; we just keep the slot in the enum so the index numbering
;; stays aligned with the C side.
(defconstant +sb-fiber-field-catch+         3)
(defconstant +sb-fiber-field-unwind+        4)
(defconstant +sb-fiber-field-return-fiber+  5)

;;; --- Fiber state constants (must match enum fiber_state in fiber.h) ---

(defconstant +fiber-new+  0)
(defconstant +fiber-runnable+ 1)
(defconstant +fiber-running+  2)
(defconstant +fiber-dead+     3)

;;; --- Lisp fiber object ---

(defstruct (fiber (:constructor %make-fiber))
  "A stackful coroutine with its own control and binding stacks."
  (sap (sb-sys:int-sap 0) :type sb-sys:system-area-pointer)
  (function nil :type (or null function))
  ;; If the fiber's entry function escapes via an unhandled condition,
  ;; the Lisp-side trampoline stores the condition here and exits
  ;; cleanly.  FIBER-SWITCH re-signals it in the caller's context on
  ;; the next switch out of the fiber, so fiber-internal conditions
  ;; never unwind across the fiber boundary (which would reach stack
  ;; frames that aren't on the fiber's own control stack -- see
  ;; IMPL-344).
  (pending-condition nil :type (or null condition)))

;;; *current-fiber* is a per-thread special: DEFVAR + SETF on a
;;; declaimed-SPECIAL symbol gives each thread its own TLS cell
;;; under sb-thread (one thread's write does not leak to another's
;;; read).  A thread that has never called MAKE-MAIN-FIBER has no
;;; TLS value for this symbol, so SYMBOL-VALUE returns the global
;;; (NIL); FIBER-SWITCH enforces that the caller has established a
;;; main fiber first.
(defvar *current-fiber* nil
  "The fiber currently running on this thread, or NIL if no main
fiber has been established on this thread.")

(declaim (inline current-fiber))
(defun current-fiber ()
  "Return the fiber currently running on this thread (or NIL if
MAKE-MAIN-FIBER has not been called on this thread)."
  *current-fiber*)

;;; Callback trampoline: called from C fiber_trampoline_c via entry_fn.
;;;
;;; Two things must happen on first entry into a new fiber:
;;;
;;;   1. The fiber inherits whatever TLS the caller had when it invoked
;;;      FIBER-SWITCH, including *HANDLER-CLUSTERS* and *RESTART-CLUSTERS*.
;;;      Those clusters' handler lambdas close over BLOCK exit points on
;;;      the caller's control stack.  If the fiber signals a condition
;;;      and one of the caller's handlers matches, the handler's
;;;      RETURN-FROM would try to unwind to a frame that is not on the
;;;      fiber's stack at all -- crashing inside SB-C::UNWIND.
;;;      Re-binding both clusters to their initial (empty) state at fiber
;;;      entry isolates the fiber from the caller's handler chain.
;;;
;;;   2. A condition that escapes the user function must not escape the
;;;      fiber itself -- the fiber's stack is about to be unwound to
;;;      completion and the return_fiber path in C expects a normal
;;;      return.  We catch anything that escapes here, stash it on the
;;;      fiber, and let FIBER-SWITCH re-signal it on the caller's stack.
(define-alien-callable sb-fiber-lisp-entry void
    ((arg unsigned-long))
  ;; ARG is the C sb_fiber* (as an integer, via the stable SAP).
  ;; Look up the Lisp wrapper in the registry.  See the registry
  ;; comment for why we don't pass the Lisp address directly.
  (let ((f (%fiber-registry-get arg)))
    (when (and f (fiber-function f))
      (let ((sb-kernel:*handler-clusters* sb-kernel::**initial-handler-clusters**)
            (sb-kernel:*restart-clusters* nil))
        (handler-case
            (funcall (fiber-function f))
          (condition (c)
            (setf (fiber-pending-condition f) c)))))))

;;; Cache the callback SAP at load time.
(defvar *lisp-entry-sap* nil)

(defun ensure-lisp-entry-sap ()
  (or *lisp-entry-sap*
      (setf *lisp-entry-sap*
            (sb-alien:alien-sap
             (alien-callable-function 'sb-fiber-lisp-entry)))))

;;; Registry mapping the fiber's C sb_fiber* (a stable mmap address)
;;; to its Lisp wrapper struct.  We cannot pass the Lisp wrapper
;;; address through the C struct's entry_arg field -- that's a raw
;;; void * invisible to GC, and any copying GC moving the wrapper
;;; would leave entry_arg pointing at the old, now-freed address.
;;; The C SAP never moves (it's malloc'd once, freed on destroy), so
;;; we use its integer value as a stable key.  The hash table's
;;; VALUE is the Lisp wrapper, which in turn keeps the wrapper's
;;; function closure alive until destroy-fiber removes the entry.
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
      (%fiber-register (sb-thread:current-thread-sap) sap)
      (%fiber-registry-put f)
      ;; Entry argument is the C SAP itself.  Crucial that this is
      ;; the stable mmap address, not the Lisp wrapper's address --
      ;; see the registry comment on why.
      (%fiber-prepare sap (ensure-lisp-entry-sap) sap)
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

;;; --- Context switch ---
;;;
;;; Lisp owns validation, catch/unwind save+pre-install,
;;; return_fiber, and *CURRENT-FIBER*.  C owns state transitions,
;;; BSP (which must be captured inside the alien call so that it
;;; sees the *SAVED-FP* binding that INVOKE-WITH-SAVED-FP added --
;;; see the comment on sb_fiber_switch_prep), the Lisp control stack
;;; swap, and the binding-stack swap.  The register/SP swap itself
;;; is the %FIBER-REGISTER-SWAP VOP, inlined at the call site so it
;;; avoids an alien-call round-trip.

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
      (let ((ts (%fiber-state-at to-sap)))
        (unless (or (= ts +fiber-runnable+) (= ts +fiber-new+))
          (error "fiber-switch: TO fiber is not RUNNABLE or NEW")))
      ;; Record the return path on the incoming fiber.
      (setf (%fiber-return-fiber-at to-sap) from-sap)
      ;; Save catch/unwind into the outgoing fiber and pre-install
      ;; the incoming fiber's catch/unwind into the thread struct.
      ;; BSP, binding-stack swap, the state transitions, and the
      ;; pseudo-atomic entry all happen in %fiber-switch-prep, which
      ;; runs inside the critical region where deferrables are
      ;; deferred (via pa_bits) and th->control_stack_* is consistent
      ;; with the swap (see sb_fiber_switch_prep for why that has to
      ;; be in C, not Lisp).
      (setf (%fiber-catch-at  from-sap) (%thread-catch  th-sap)
            (%fiber-unwind-at from-sap) (%thread-unwind th-sap))
      (setf (%thread-catch  th-sap) (%fiber-catch-at  to-sap)
            (%thread-unwind th-sap) (%fiber-unwind-at to-sap))
      (setf *current-fiber* to)
      ;; Switch = prep (alien C) + register/SP swap (VOP).  PREP
      ;; enters pseudo-atomic and stages the BSP/bounds/state/
      ;; binding swap.  The VOP inlines the ~14-instruction register
      ;; save + SP load + RET (avoiding the C round-trip that used
      ;; to go through fiber_swap_context), and its RESUME tail
      ;; emits the inline exit-PA sequence so we pay no alien call
      ;; to restore the mask -- and in fact no sigprocmask at all:
      ;; interrupts are deferred via pa_bits for the duration of
      ;; the critical region.
      (%fiber-switch-prep from-sap to-sap)
      (%fiber-register-swap from-sap to-sap)
      ;; Resumed here, eventually, as FROM.  Whoever switched back to
      ;; us already restored the thread struct with this same
      ;; pre-install pattern.
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

) ; #+(and (or x86-64 arm64) (not win32)) (progn ...
