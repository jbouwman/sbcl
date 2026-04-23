;;;; Tests for sb-fiber (pure -- no side effects on filesystem)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(unless (and (member :sb-thread *features*)
             (or (member :x86-64 *features*) (member :arm64 *features*))
             (not (member :win32 *features*)))
  (invoke-restart 'run-tests::skip-file))

(require :sb-fiber)
(use-package :sb-fiber)

;;; --- Stage 1 tests: allocation ---

(with-test (:name (:fiber :creation-and-destroy))
  (let ((f (make-fiber (lambda ()))))
    (assert (fiber-alive-p f))
    (destroy-fiber f)
    (assert (not (fiber-alive-p f)))))

(with-test (:name (:fiber :with-fiber-cleanup))
  (let ((saved nil))
    (with-fiber (f (lambda ()))
      (assert (fiber-alive-p f))
      (setf saved f))
    (assert (not (fiber-alive-p saved)))))

(with-test (:name (:fiber :multiple-creation))
  (let ((fibers (loop repeat 100
                      collect (make-fiber (lambda ())))))
    (assert (= 100 (length fibers)))
    (assert (every #'fiber-alive-p fibers))
    (mapc #'destroy-fiber fibers)
    (assert (notany #'fiber-alive-p fibers))))

(with-test (:name (:fiber :double-destroy-is-safe))
  (let ((f (make-fiber (lambda ()))))
    (destroy-fiber f)
    (destroy-fiber f) ; should be a no-op
    (assert (not (fiber-alive-p f)))))

(with-test (:name (:fiber :custom-stack-sizes))
  (let ((f (make-fiber (lambda ()) :stack-size 131072 :binding-stack-size 16384)))
    (assert (fiber-alive-p f))
    (destroy-fiber f)))

(with-test (:name (:fiber :main-fiber-creation))
  (let ((mf (make-main-fiber)))
    (assert (fiber-alive-p mf))
    (destroy-fiber mf)
    (assert (not (fiber-alive-p mf)))))

;;; --- Stage 2+3 tests: switching ---

(with-test (:name (:fiber :basic-switch-roundtrip))
  (let* ((main (make-main-fiber))
         (executed nil)
         (child nil))
    (setf child
          (make-fiber
           (lambda ()
             (setf executed t)
             (fiber-switch child main))))
    (fiber-switch main child)
    (assert executed () "Fiber body did not execute")
    (destroy-fiber child)
    (destroy-fiber main)))

(with-test (:name (:fiber :ping-pong-switch))
  (let* ((main (make-main-fiber))
         (count 0)
         (child nil))
    (setf child
          (make-fiber
           (lambda ()
             (dotimes (i 100)
               (incf count)
               (fiber-switch child main)))))
    (dotimes (i 100)
      (fiber-switch main child))
    (assert (= count 100) () "Expected 100 switches, got ~D" count)
    (destroy-fiber child)
    (destroy-fiber main)))

(with-test (:name (:fiber :return-value-preserved))
  (let* ((main (make-main-fiber))
         (results nil)
         (child nil))
    (setf child
          (make-fiber
           (lambda ()
             ;; Push values before each yield
             (push :first results)
             (fiber-switch child main)
             (push :second results)
             (fiber-switch child main)
             (push :third results)
             (fiber-switch child main))))
    (fiber-switch main child)
    (fiber-switch main child)
    (fiber-switch main child)
    (assert (equal (reverse results) '(:first :second :third)))
    (destroy-fiber child)
    (destroy-fiber main)))

(with-test (:name (:fiber :deep-call-chain-switch))
  (let* ((main (make-main-fiber))
         (depth-reached nil)
         (child nil))
    (labels ((deep-call (n)
               (if (zerop n)
                   (progn
                     (setf depth-reached t)
                     (fiber-switch child main))
                   (deep-call (1- n)))))
      (setf child
            (make-fiber
             (lambda ()
               (deep-call 50)
               (fiber-switch child main))))
      (fiber-switch main child)
      (assert depth-reached () "Deep call chain did not reach bottom")
      ;; Switch back to let it finish
      (fiber-switch main child)
      (destroy-fiber child)
      (destroy-fiber main))))

(with-test (:name (:fiber :multiple-fibers-round-robin))
  (let* ((main (make-main-fiber))
         (n 10)
         (counters (make-array n :initial-element 0))
         (fibers (make-array n)))
    ;; Create N fibers, each increments its counter and yields back to main
    (dotimes (i n)
      (let ((idx i))
        (setf (aref fibers i)
              (make-fiber
               (lambda ()
                 (loop
                   (incf (aref counters idx))
                   (fiber-switch (aref fibers idx) main)))))))
    ;; Round-robin: switch to each fiber 20 times
    (dotimes (round 20)
      (dotimes (i n)
        (fiber-switch main (aref fibers i))))
    ;; Each fiber should have run 20 times
    (dotimes (i n)
      (assert (= (aref counters i) 20) ()
              "Fiber ~D ran ~D times, expected 20" i (aref counters i)))
    (dotimes (i n) (destroy-fiber (aref fibers i)))
    (destroy-fiber main)))

;;; --- Stage 3 tests: special variable isolation ---

(defvar *fiber-test-var* :main-value)

(with-test (:name (:fiber :special-variable-isolation))
  (let* ((main (make-main-fiber))
         (fiber-saw nil)
         (child nil))
    (setf child
          (make-fiber
           (lambda ()
             ;; Fiber sees the thread's current binding
             (setf fiber-saw *fiber-test-var*)
             (fiber-switch child main))))
    (let ((*fiber-test-var* :rebound))
      (fiber-switch main child)
      ;; After switch back, our binding should be intact
      (assert (eq *fiber-test-var* :rebound)))
    (assert (eq fiber-saw :rebound)
            () "Fiber saw ~S instead of :REBOUND" fiber-saw)
    (destroy-fiber child)
    (destroy-fiber main)))

(with-test (:name (:fiber :let-bindings-survive-switch))
  (let* ((main (make-main-fiber))
         (child nil)
         (result nil))
    (setf child
          (make-fiber
           (lambda ()
             (let ((x 42)
                   (y "hello")
                   (z (list 1 2 3)))
               (fiber-switch child main)
               ;; After resuming, locals should still be intact
               (setf result (list x y z))
               (fiber-switch child main)))))
    (fiber-switch main child)  ; run until first yield
    (fiber-switch main child)  ; resume, captures result, yields again
    (assert (equal result '(42 "hello" (1 2 3))))
    (destroy-fiber child)
    (destroy-fiber main)))

(with-test (:name (:fiber :loop-state-survives-switch))
  (let* ((main (make-main-fiber))
         (child nil)
         (sum 0))
    (setf child
          (make-fiber
           (lambda ()
             (dotimes (i 10)
               (incf sum i)
               (fiber-switch child main))
             (fiber-switch child main))))
    ;; Drive the fiber through all 10 iterations + final yield
    (dotimes (_ 11)
      (fiber-switch main child))
    ;; sum of 0..9 = 45
    (assert (= sum 45) () "Expected 45, got ~D" sum)
    (destroy-fiber child)
    (destroy-fiber main)))

;;; --- handler-case across switches ---

(with-test (:name (:fiber :handler-case-across-switch))
  (let* ((main (make-main-fiber))
         (child nil)
         (caught nil))
    (setf child
          (make-fiber
           (lambda ()
             (handler-case
                 (progn
                   (fiber-switch child main)  ; yield inside handler-case
                   (error "test error"))
               (error (c)
                 (setf caught (princ-to-string c))))
             (fiber-switch child main))))
    (fiber-switch main child)  ; enter handler-case, yield
    (fiber-switch main child)  ; resume, signal error, catch it, yield
    (assert (string= caught "test error"))
    (destroy-fiber child)
    (destroy-fiber main)))

;;; --- unwind-protect across switches ---

(with-test (:name (:fiber :unwind-protect-across-switch))
  (let* ((main (make-main-fiber))
         (child nil)
         (cleanup-ran nil))
    (setf child
          (make-fiber
           (lambda ()
             (unwind-protect
                  (progn
                    (fiber-switch child main)  ; yield inside unwind-protect
                    ;; Normal exit after resume
                    )
               (setf cleanup-ran t))
             (fiber-switch child main))))
    (fiber-switch main child)  ; enter unwind-protect body, yield
    (fiber-switch main child)  ; resume, exit normally, cleanup runs, yield
    (assert cleanup-ran () "unwind-protect cleanup did not run")
    (destroy-fiber child)
    (destroy-fiber main)))

;;; --- stack-bounds: thread->control_stack_{start,end} must reflect
;;; --- the running fiber's stack, so STACK-ALLOCATED-P / the
;;; --- COPY-CTYPE AVER work on DX-allocated objects inside a fiber.

(with-test (:name (:fiber :stack-bounds-track-fiber))
  (let* ((main (make-main-fiber))
         (err nil)
         (child (make-fiber
                 (lambda ()
                   (handler-case
                       ;; The hashset path in SB-KERNEL::COPY-CTYPE
                       ;; AVERs (STACK-ALLOCATED-P X) on a freshly
                       ;; DX-allocated ctype.  A wrong control-stack
                       ;; range makes the AVER fire.
                       (loop for i from 1 to 20
                             do (typep i `(or (member ,i)
                                              (member ,(1+ i)))))
                     (error (c) (setf err (princ-to-string c))))))))
    (fiber-switch main child)
    (assert (null err) () "Expected no error, got: ~A" err)
    (destroy-fiber child)
    (destroy-fiber main)))

;;; --- thread->binding-stack-start must also track the active fiber.
;;; --- BINDING-STACK-USAGE and the debugger's walk-binding-stack both
;;; --- compute (bsp - bsp-start).  If bsp-start still points at the
;;; --- main thread's binding stack while a child fiber runs, the
;;; --- reported depth is a huge bogus number (cross-region subtraction).

(with-test (:name (:fiber :binding-stack-start-tracks-fiber))
  (let* ((main (make-main-fiber))
         (seen-usage nil)
         (child (make-fiber
                 (lambda ()
                   (setf seen-usage (sb-kernel::binding-stack-usage))
                   (fiber-switch sb-fiber::*current-fiber* main)))))
    (fiber-switch main child)
    ;; The child's binding stack is at most a few KB deep; the bug
    ;; produced deltas on the order of megabytes.
    (assert (and (<= 0 seen-usage)
                 (< seen-usage 65536))
            ()
            "BINDING-STACK-USAGE reported ~D from inside fiber; expected < 64 KiB"
            seen-usage)
    (destroy-fiber child)
    (destroy-fiber main)))

;;; --- Backtrace / debugger walk from inside a fiber.  The stack
;;; --- walker uses *control-stack-start*, *control-stack-end*, and
;;; --- *binding-stack-start*; with all three fixed, SB-DEBUG:
;;; --- LIST-BACKTRACE must terminate cleanly at the fiber's asm
;;; --- trampoline instead of walking into unmapped memory.

(with-test (:name (:fiber :backtrace-inside-fiber))
  (let* ((main (make-main-fiber))
         (frames nil)
         (child (make-fiber
                 (lambda ()
                   (setf frames (sb-debug:list-backtrace :count 20))))))
    (fiber-switch main child)
    ;; Any non-zero frame count with no crash is a win; typical runs
    ;; see ~4 (user lambda, alien-callable wrapper, trampoline glue,
    ;; asm stub).
    (assert (and frames (integerp (length frames)) (< (length frames) 50))
            () "unexpected backtrace from fiber: ~S" frames)
    (destroy-fiber child)
    (destroy-fiber main)))

