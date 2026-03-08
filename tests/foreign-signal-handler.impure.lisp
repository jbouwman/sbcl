;;;; Tests for signal handler chaining when foreign libraries install
;;;; their own signal handlers and create non-Lisp threads.

(use-package :test-util)

(compile-so "foreign-signal-handler.c" "foreign-signal-handler.so")

(sb-alien:define-alien-type nil
  (sb-alien:struct sigaction-raw
    (sa-handler sb-alien:unsigned-long)
    (sa-flags sb-alien:unsigned-long)
    (sa-restorer sb-alien:unsigned-long)
    (sa-mask (array sb-alien:unsigned-long 16))))

(sb-alien:define-alien-routine ("sigaction" %sigaction) sb-alien:int
  (signum sb-alien:int)
  (act (* (sb-alien:struct sigaction-raw)))
  (oldact (* (sb-alien:struct sigaction-raw))))

(sb-alien:define-alien-routine foreign-install-sigsegv-handler sb-alien:int)
(sb-alien:define-alien-routine foreign-get-sigsegv-handler sb-alien:unsigned-long)
(sb-alien:define-alien-routine foreign-get-our-handler-addr sb-alien:unsigned-long)
(sb-alien:define-alien-routine foreign-handler-was-called sb-alien:int)
(sb-alien:define-alien-routine foreign-reset-handler-flag sb-alien:void)
(sb-alien:define-alien-routine foreign-spawn-worker-thread sb-alien:int)
(sb-alien:define-alien-routine foreign-spawn-fault-thread sb-alien:int)
(sb-alien:define-alien-routine foreign-release-worker-thread sb-alien:void)
(sb-alien:define-alien-routine foreign-join-worker-thread sb-alien:int)

(sb-alien:define-alien-routine save-foreign-signal-handler sb-alien:int
  (signal sb-alien:int))

(defconstant +SIGSEGV+ 11)
(defconstant +SIG-DFL+ 0)

(defun get-current-sigsegv-handler ()
  (let ((act (sb-alien:make-alien (sb-alien:struct sigaction-raw))))
    (unwind-protect
         (progn
           (%sigaction +SIGSEGV+ nil act)
           (sb-alien:slot act 'sa-handler))
      (sb-alien:free-alien act))))

(defvar *sbcl-original-handler* (get-current-sigsegv-handler))

(defun restore-sbcl-handler ()
  "Restore SBCL's original SIGSEGV handler."
  (let ((restore (sb-alien:make-alien (sb-alien:struct sigaction-raw))))
    (unwind-protect
         (progn
           (%sigaction +SIGSEGV+ nil restore)
           (setf (sb-alien:slot restore 'sa-handler) *sbcl-original-handler*)
           (%sigaction +SIGSEGV+ restore nil))
      (sb-alien:free-alien restore))))

(with-test (:name :foreign-handler-installation
            :skipped-on (not :sb-thread))
  (assert (/= *sbcl-original-handler* +SIG-DFL+))
  (assert (= 0 (foreign-install-sigsegv-handler)))
  (let ((new-handler (get-current-sigsegv-handler)))
    (assert (/= new-handler *sbcl-original-handler*))
    (assert (= new-handler (foreign-get-our-handler-addr)))
    (restore-sbcl-handler)))

(with-test (:name :handler-save-restore-cycle
            :skipped-on (not :sb-thread))
  (let ((original (get-current-sigsegv-handler))
        (saved (sb-alien:make-alien (sb-alien:struct sigaction-raw))))
    (unwind-protect
         (progn
           (%sigaction +SIGSEGV+ nil saved)
           (%sigaction +SIGSEGV+ saved nil)
           (assert (= (get-current-sigsegv-handler) original)))
      (sb-alien:free-alien saved))))

(with-test (:name :foreign-handler-removed-from-non-lisp-thread
            :skipped-on (not :sb-thread))
  (assert (= 0 (foreign-install-sigsegv-handler)))
  (assert (= (get-current-sigsegv-handler) (foreign-get-our-handler-addr)))
  (assert (= 0 (foreign-spawn-worker-thread)))
  (restore-sbcl-handler)
  (foreign-release-worker-thread)
  (let ((result (foreign-join-worker-thread)))
    ;; sigaction is process-wide, so the worker sees SBCL's handler
    (assert (= result -2))))

(with-test (:name :foreign-handler-chaining-on-non-lisp-thread
            :skipped-on (not :sb-thread))
  ;; Install foreign handler
  (assert (= 0 (foreign-install-sigsegv-handler)))
  ;; Save it in SBCL's foreign_sigactions array
  (assert (= 0 (save-foreign-signal-handler +SIGSEGV+)))
  ;; Restore SBCL's handler (removes foreign handler process-wide)
  (restore-sbcl-handler)
  ;; Spawn a non-Lisp thread that triggers SIGSEGV
  (assert (= 0 (foreign-spawn-fault-thread)))
  (foreign-release-worker-thread)
  (let ((result (foreign-join-worker-thread)))
    (assert (= result 1))
    (assert (= 1 (foreign-handler-was-called)))))

(with-test (:name :load-shared-object-auto-saves-handlers
            :skipped-on (not :sb-thread))
  (assert (= 0 (foreign-install-sigsegv-handler)))
  #-(or win32 (not sb-thread))
  (sb-alien::save-foreign-signal-handlers)
  (restore-sbcl-handler)
  (foreign-reset-handler-flag)
  (assert (= 0 (foreign-spawn-fault-thread)))
  (foreign-release-worker-thread)
  (let ((result (foreign-join-worker-thread)))
    (assert (= result 1))
    (assert (= 1 (foreign-handler-was-called)))))
