;;;; Fiber integration, threading and stress tests for per-process heaps.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

(unless (and (member :sb-thread *features*)
             (member :sb-process-heaps *features*)
             (member :sb-fiber *features*))
  (invoke-restart 'run-tests::skip-file))

(require :sb-fiber)
(use-package :sb-fiber)

;;; Errors that escape a test must not be recorded by the harness while
;;; the test's heap is still installed, or the failure records themselves
;;; become dangling process objects once the heap is released.
(defmacro with-test-heap ((var &rest args) &body body)
  `(let ((,var (make-heap ,@args)))
     (unwind-protect
          (handler-bind ((error (lambda (c)
                                  (declare (ignore c))
                                  (sb-fiber::%switch-heap 0))))
            ,@body)
       (release-heap ,var))))

;;; --- Fibers with heaps ---

(with-test (:name (:process-heap :fiber :allocates-in-own-heap))
  (with-fiber-thread ()
    (let* ((f (make-fiber (lambda ()
                            (let ((data (make-list 100)))
                              (yield-fiber (object-heap data))
                              (object-heap (make-array 5))))
                          :heap t :name "worker"))
           (h (fiber-heap f)))
      (assert (heap-p h))
      (assert (eq (heap-fiber h) f))
      (assert (eq (resume-fiber f) h))
      (assert (eq (resume-fiber f) h))
      (assert (null (current-heap)))
      (release-fiber f)
      (assert (not (heap-alive-p h))))))

(with-test (:name (:process-heap :fiber :explicit-heap))
  (with-fiber-thread ()
    (let* ((h (make-heap :name "explicit"))
           (f (make-fiber (lambda () (object-heap (list 1))) :heap h)))
      (assert (eq (fiber-heap f) h))
      (assert (eq (join-fiber f) h))
      (assert-error (make-fiber (lambda ()) :heap h))
      (release-fiber f)
      (assert (not (heap-alive-p h))))))

(with-test (:name (:process-heap :fiber :values-cross-boundary-by-copy))
  (with-fiber-thread ()
    (let* ((f (make-fiber (lambda ()
                            (let ((v (yield-fiber (list 1 2) (vector 3))))
                              (list :got v (object-heap v))))
                          :heap t))
           (h (fiber-heap f)))
      (multiple-value-bind (a b) (resume-fiber f)
        (assert (equal a '(1 2)))
        (assert (equalp b #(3)))
        ;; Values are copies in the global heap.
        (assert (null (object-heap a)))
        (assert (null (object-heap b))))
      (let ((global (list :g)))
        (let ((result (resume-fiber f global)))
          (assert (eq (second result) global))
          (assert (null (third result)))
          (assert (null (object-heap result)))))
      (assert (not (fiber-alive-p f)))
      (release-fiber f)
      (assert (not (heap-alive-p h))))))

(defun heap-worker-function ()
  (list :worker :done))

(with-test (:name (:process-heap :fiber :symbol-function-designator))
  (with-fiber-thread ()
    (let ((f (make-fiber 'heap-worker-function :heap t)))
      (assert (equal (join-fiber f) '(:worker :done)))
      (release-fiber f))))

(with-test (:name (:process-heap :fiber :closure-from-process-rejected))
  (with-fiber-thread ()
    (let ((outer (make-fiber (lambda ()
                               (let ((n (list 1)))
                                 (handler-case
                                     (progn (make-fiber (lambda () n) :heap t) :created)
                                   (cross-heap-reference () :rejected))))
                             :heap t)))
      (assert (eq (join-fiber outer) :rejected))
      (release-fiber outer))))

(with-test (:name (:process-heap :fiber :escaping-condition-is-copied))
  (with-fiber-thread ()
    (let ((f (make-fiber (lambda ()
                           (error "boom ~A" (make-string 3 :initial-element #\!)))
                         :heap t)))
      (handler-case (progn (resume-fiber f) (error "no condition"))
        (simple-error (c)
          (assert (null (object-heap c)))
          (assert (search "boom !!!" (princ-to-string c)))))
      (release-fiber f))))

(defvar *strict-fiber-store-target* (list :unchanged))

(with-test (:name (:process-heap :fiber :strict-store-error-can-return))
  (with-fiber-thread ()
    (let ((f (make-fiber
              (lambda ()
                (block handled
                  (handler-bind
                      ((process-heap-store-error
                         (lambda (condition)
                           (return-from handled
                             (list :caught
                                   (process-heap-store-error-kind condition))))))
                    (setf (car *strict-fiber-store-target*) (list :written))
                    :store-succeeded)))
              :heap (make-heap :strict t))))
      (assert (equal (resume-fiber f) '(:caught :escape)))
      (assert (equal *strict-fiber-store-target* '(:unchanged)))
      (release-fiber f))))

(with-test (:name (:process-heap :fiber :hard-limit-signals-in-fiber))
  (with-fiber-thread ()
    (let ((f (make-fiber
              (lambda ()
                (block exhausted
                  (handler-bind
                      ((process-heap-exhausted-error
                         (lambda (condition)
                           (declare (ignore condition))
                           (return-from exhausted :caught))))
                    (let ((objects nil))
                      (loop repeat 200000
                            do (push (make-string 256 :initial-element #\y)
                                     objects)))
                    :not-exhausted)))
              :heap (make-heap :hard-limit (* 4 1024 1024)))))
      (assert (eq (resume-fiber f) :caught))
      (release-fiber f))))

;;; --- Hard-limit exhaustion and deferred interrupt state ---

;;; The allocation slow path may ask for a local collection of the heap
;;; it then finds exhausted, in the same call: it sets
;;; pseudo-atomic-interrupted and blocks deferrable signals until the
;;; trap at the end of the pseudo-atomic section.  Exhaustion switches
;;; the heap out, so that request can no longer be found and has to be
;;; retracted.  What must survive the retraction: deferrable signals end
;;; up unblocked, pseudo-atomic-interrupted ends up clear, and pending
;;; work that has nothing to do with the heap still runs when it is due.

(sb-alien:define-alien-routine "deferrables_blocked_p" sb-alien:int
  (sigset sb-alien:unsigned-long))

(defun deferrable-signals-blocked-p ()
  ;; A C bool: only the low byte is defined.
  (logtest #xff (deferrables-blocked-p 0)))

(defun pseudo-atomic-interrupted-p ()
  (logtest (sb-sys:sap-ref-word (sb-thread:current-thread-sap)
                                (ash sb-vm::thread-pseudo-atomic-bits-slot
                                     sb-vm:word-shift))
           #+arm64 sb-vm::pseudo-atomic-interrupted-flag
           #-arm64 1))

(defmacro spin-until (form &optional (seconds 10))
  `(loop with deadline = (+ (get-internal-real-time)
                            (* ,seconds internal-time-units-per-second))
         until ,form
         do (assert (< (get-internal-real-time) deadline) ()
                    "timed out waiting for ~S" ',form)))

;;; MAKE-STRING is flushable, so a discarded one is deleted outright.
(declaim (notinline keep-alive))
(defun keep-alive (object) object)

(defun exhaust-installed-heap ()
  "Reach the installed heap's hard limit on an allocation that asks for
a collection of that heap first, and handle the resulting error within
the heap's dynamic extent.  Returns :CAUGHT.

The sequence is what makes the collection request and the exhaustion
land in the same slow-path call, which is the case the retraction is
for.  HEAP-GC leaves nothing claimed since the last collection, so the
next slow-path allocation asks for nothing and claims a block; the one
after it is over the threshold, so it asks for a collection -- and is
also the one that does not fit under the hard limit.  The handler is
established first so that establishing it cannot allocate in between."
  (block exhausted
    (handler-bind ((process-heap-exhausted-error
                     (lambda (condition)
                       (declare (ignore condition))
                       (return-from exhausted :caught))))
      (heap-gc)
      (keep-alive (make-string 64 :initial-element #\p))
      (keep-alive (make-string (* 1024 1024) :initial-element #\x)))
    :not-exhausted))

(defvar *interruption-ran* nil)

(defun note-interruption ()
  (setf *interruption-ran* t))

(with-test (:name (:process-heap :hard-limit :retraction-unblocks-deferrables))
  (with-test-heap (h :gc-threshold 1 :hard-limit (* 1024 1024))
    (with-heap (h)
      (assert (eq (exhaust-installed-heap) :caught))
      (assert (eq (current-heap) h))
      (assert (not (pseudo-atomic-interrupted-p)))
      (assert (not (deferrable-signals-blocked-p))))
    ;; A deferrable signal is still delivered afterwards.
    (setf *interruption-ran* nil)
    (let ((self sb-thread:*current-thread*))
      (sb-thread:join-thread
       (sb-thread:make-thread
        (lambda () (sb-thread:interrupt-thread self #'note-interruption)))))
    (spin-until *interruption-ran*)
    (assert (not (deferrable-signals-blocked-p)))
    (assert (not (pseudo-atomic-interrupted-p)))))

(with-test (:name (:process-heap :hard-limit :retraction-keeps-deferred-handler))
  (with-test-heap (h :gc-threshold 1 :hard-limit (* 1024 1024))
    (setf *interruption-ran* nil)
    (let* ((self sb-thread:*current-thread*)
           (ready (sb-thread:make-semaphore))
           (interrupter (sb-thread:make-thread
                         (lambda ()
                           (sb-thread:wait-on-semaphore ready)
                           (sb-thread:interrupt-thread self #'note-interruption)))))
      (sb-sys:without-interrupts
        (sb-thread:signal-semaphore ready)
        (spin-until sb-sys:*interrupt-pending*)
        (assert (not *interruption-ran*))
        (with-heap (h)
          (assert (eq (exhaust-installed-heap) :caught))
          (assert (eq (current-heap) h)))
        ;; The interruption is still deferred, and nothing is left
        ;; trapping on every allocation.
        (assert (not *interruption-ran*))
        (assert sb-sys:*interrupt-pending*)
        (assert (not (pseudo-atomic-interrupted-p))))
      ;; Leaving WITHOUT-INTERRUPTS runs it.
      (spin-until *interruption-ran*)
      (assert (not (pseudo-atomic-interrupted-p)))
      (assert (not (deferrable-signals-blocked-p)))
      (sb-thread:join-thread interrupter))))

(with-test (:name (:process-heap :hard-limit :retraction-keeps-gc-pending))
  (with-test-heap (h :gc-threshold 1 :hard-limit (* 1024 1024))
    (let ((epoch sb-kernel::*gc-epoch*))
      (sb-sys:without-gcing
        (sb-ext:gc)
        (assert sb-kernel:*gc-pending*)
        (with-heap (h)
          (assert (eq (exhaust-installed-heap) :caught))
          (assert (eq (current-heap) h)))
        (assert sb-kernel:*gc-pending*))
      ;; Leaving WITHOUT-GCING runs the global collection.
      (assert (not sb-kernel:*gc-pending*))
      (assert (not (eq epoch sb-kernel::*gc-epoch*)))
      (assert (not (pseudo-atomic-interrupted-p)))
      (assert (not (deferrable-signals-blocked-p))))))

(with-test (:name (:process-heap :hard-limit :retraction-keeps-stop-for-gc-pending))
  (with-test-heap (h :gc-threshold 1 :hard-limit (* 1024 1024))
    (let* ((ready (sb-thread:make-semaphore))
           (collector (sb-thread:make-thread
                       (lambda ()
                         (sb-thread:wait-on-semaphore ready)
                         (sb-ext:gc :full t)
                         :collected))))
      (sb-sys:without-gcing
        (sb-thread:signal-semaphore ready)
        (spin-until sb-kernel:*stop-for-gc-pending*)
        (with-heap (h)
          (assert (eq (exhaust-installed-heap) :caught))
          (assert (eq (current-heap) h)))
        (assert sb-kernel:*stop-for-gc-pending*))
      ;; Leaving WITHOUT-GCING stops this thread for the other's collection.
      (assert (eq (sb-thread:join-thread collector) :collected))
      (assert (not sb-kernel:*stop-for-gc-pending*))
      (assert (not (pseudo-atomic-interrupted-p)))
      (assert (not (deferrable-signals-blocked-p))))))

(with-test (:name (:process-heap :fiber :interrupt-with-local-condition))
  (with-fiber-thread ()
    (let* ((f (make-fiber (lambda ()
                            (handler-case (loop (yield-fiber))
                              (simple-error (c) (princ-to-string c))))
                          :heap t))
           (h (fiber-heap f)))
      (resume-fiber f)
      (let ((c (with-heap (h) (make-condition 'simple-error
                                              :format-control "stop ~A"
                                              :format-arguments (list (list 1))))))
        (interrupt-fiber f c)
        (assert (null (object-heap (fiber-condition f))))
        (let ((result (resume-fiber f)))
          (assert (search "stop (1)" result))))
      (release-fiber f))))

(with-test (:name (:process-heap :fiber :auto-gc-inside-fiber))
  (with-fiber-thread ()
    (let* ((f (make-fiber (lambda ()
                            (let ((keep (make-array 20 :initial-element nil)))
                              (dotimes (i 200000)
                                (setf (aref keep (mod i 20)) (make-list 10 :initial-element i))
                                (when (zerop (mod i 50000)) (yield-fiber i)))
                              (loop for x across keep sum (length x))))
                          :heap (make-heap :gc-threshold (* 64 1024)))))
      (loop while (fiber-alive-p f) do (resume-fiber f))
      (assert (> (heap-gc-count (fiber-heap f)) 0))
      (release-fiber f))))

(defun verify-heap-of (fiber)
  ;; The fiber is dead, so install its heap here to verify it.
  (with-heap ((fiber-heap fiber))
    (verify-heap)))

(with-test (:name (:process-heap :fiber :many-fibers-many-heaps))
  (with-fiber-thread ()
    (let* ((n 100)
           (fibers (loop for i below n
                         collect (let ((i i))
                                   (make-fiber (lambda ()
                                                 (let ((data (make-list 200 :initial-element i)))
                                                   (dotimes (round 3)
                                                     (dotimes (j 2000) (make-array 8))
                                                     (heap-gc)
                                                     (yield-fiber (reduce #'+ data)))
                                                   (reduce #'+ data)))
                                               :heap (make-heap :gc-threshold (* 64 1024)))))))
      (dotimes (round 4)
        (loop for f in fibers for i from 0
              do (assert (= (resume-fiber f) (* 200 i))))
        (sb-ext:gc :full t))
      (dolist (f fibers)
        (assert (not (fiber-alive-p f)))
        (assert (null (verify-heap-of f)))
        (release-fiber f)))))

;;; --- Copying structures and standard objects ---

(defstruct heap-test-struct a (b 0d0 :type double-float) (c 0 :type fixnum))

(with-test (:name (:process-heap :copy-for-transfer :structures-and-hash-tables))
  (with-test-heap (h)
    (let (src)
      (with-heap (h)
        (let ((table (make-hash-table :test 'equal)))
          (setf (gethash "a" table) (list 1)
                (gethash "b" table) 2)
          (setf src (list table
                          (make-heap-test-struct :a (list 1) :b 2.5d0 :c 7)))))
      (let ((copy (copy-for-transfer src)))
        (let ((table (first copy)))
          (assert (null (object-heap table)))
          (assert (equal (gethash "a" table) '(1)))
          (assert (null (object-heap (gethash "a" table))))
          (assert (eql (gethash "b" table) 2))
          (assert (eq (hash-table-test table) 'equal)))
        (let ((s (second copy)))
          (assert (heap-test-struct-p s))
          (assert (null (object-heap s)))
          (assert (equal (heap-test-struct-a s) '(1)))
          (assert (null (object-heap (heap-test-struct-a s))))
          (assert (= (heap-test-struct-b s) 2.5d0))
          (assert (= (heap-test-struct-c s) 7)))))))

(defclass heap-test-class ()
  ((x :initarg :x :accessor heap-test-class-x)
   (y :initarg :y :accessor heap-test-class-y)))

(with-test (:name (:process-heap :copy-for-transfer :standard-objects))
  (with-test-heap (h)
    (let (src)
      (with-heap (h)
        (setf src (make-instance 'heap-test-class :x (list 1 2) :y "y")))
      (let ((copy (copy-for-transfer src)))
        (assert (typep copy 'heap-test-class))
        (assert (null (object-heap copy)))
        (assert (equal (heap-test-class-x copy) '(1 2)))
        (assert (null (object-heap (heap-test-class-x copy))))
        (assert (string= (heap-test-class-y copy) "y"))))))

;;; --- Message passing between fibers ---

(with-test (:name (:process-heap :fiber :ping-pong))
  (with-fiber-thread ()
    (let* ((ping (make-fiber (lambda ()
                               (dotimes (i 100)
                                 (loop until (plusp (heap-mailbox-count (current-heap)))
                                       do (yield-fiber))
                                 (multiple-value-bind (msg found) (receive-message)
                                   (assert found)
                                   (assert (= (first msg) i))
                                   (send-message (second msg) (list i :pong))))
                               :ping-done)
                             :heap t))
           (pong (make-fiber (lambda ()
                               (dotimes (i 100)
                                 (send-message ping (list i (current-heap)))
                                 (loop until (plusp (heap-mailbox-count (current-heap)))
                                       do (yield-fiber))
                                 (multiple-value-bind (msg found) (receive-message)
                                   (assert found)
                                   (assert (equal msg (list i :pong)))))
                               :pong-done)
                             :heap t)))
      (loop while (or (fiber-alive-p ping) (fiber-alive-p pong))
            do (when (fiber-alive-p ping) (resume-fiber ping))
               (when (fiber-alive-p pong) (resume-fiber pong)))
      (release-fiber ping)
      (release-fiber pong))))

;;; --- Threads ---

(with-test (:name (:process-heap :threads :independent-collections))
  (let* ((nthreads 4)
         (threads
           (loop for t-index below nthreads
                 collect (sb-thread:make-thread
                          (lambda ()
                            (let ((h (make-heap :gc-threshold (* 256 1024))))
                              (unwind-protect
                                   (with-heap (h)
                                     (let ((keep (make-array 100 :initial-element nil)))
                                       (dotimes (i 300000)
                                         (setf (aref keep (mod i 100))
                                               (list i (make-string 8 :initial-element #\t))))
                                       (heap-gc)
                                       (dotimes (i 100)
                                         (assert (= (length (second (aref keep i))) 8)))
                                       (assert (null (verify-heap h)))
                                       (heap-gc-count h)))
                                (release-heap h))))
                          :name (format nil "heap-thread-~D" t-index)))))
    (let ((gc-thread (sb-thread:make-thread
                      (lambda () (dotimes (i 10) (sb-ext:gc :full t) (sleep 0.01))))))
      (dolist (th threads)
        (assert (> (sb-thread:join-thread th) 0)))
      (sb-thread:join-thread gc-thread))))

(with-test (:name (:process-heap :threads :cross-thread-send))
  (let* ((h (make-heap))
         (sender (sb-thread:make-thread
                  (lambda ()
                    (let ((mine (make-heap)))
                      (unwind-protect
                           (with-heap (mine)
                             (dotimes (i 1000)
                               (send-message h (list i (make-array 10 :initial-element i)))))
                        (release-heap mine)))
                    :sent))))
    (assert (eq (sb-thread:join-thread sender) :sent))
    (assert (= (heap-mailbox-count h) 1000))
    (with-heap (h)
      (dotimes (i 1000)
        (multiple-value-bind (msg found) (receive-message)
          (assert found)
          (assert (= (first msg) i))
          (assert (= (aref (second msg) 9) i))
          (assert (eq (object-heap msg) h))))
      (heap-gc)
      (assert (null (verify-heap))))
    (release-heap h)))

(with-test (:name (:process-heap :threads :heap-in-use-on-other-thread))
  (let* ((h (make-heap))
         (started (sb-thread:make-semaphore))
         (release (sb-thread:make-semaphore))
         (holder (sb-thread:make-thread
                  (lambda ()
                    (with-heap (h)
                      (sb-thread:signal-semaphore started)
                      (sb-thread:wait-on-semaphore release))))))
    (sb-thread:wait-on-semaphore started)
    (assert-error (with-heap (h) nil) heap-in-use-error)
    (assert-error (release-heap h) heap-in-use-error)
    (sb-thread:signal-semaphore release)
    (sb-thread:join-thread holder)
    (release-heap h)))

(with-test (:name (:process-heap :fiber :migrate-with-heap))
  (let* ((done (sb-thread:make-semaphore))
         (fiber nil)
         (owner (sb-thread:make-thread
                 (lambda ()
                   (with-fiber-thread ()
                     (setf fiber (make-fiber (lambda ()
                                               (let ((data (make-list 1000 :initial-element :d)))
                                                 (yield-fiber :first)
                                                 (dotimes (i 5000) (make-array 3))
                                                 (heap-gc)
                                                 (list (length data) (object-heap data))))
                                             :heap t))
                     (assert (eq (resume-fiber fiber) :first))
                     (sb-thread:signal-semaphore done)
                     ;; Keep the thread alive until the fiber has migrated.
                     (sleep 0.5))))))
    (sb-thread:wait-on-semaphore done)
    (with-fiber-thread ()
      (fiber-migrate fiber sb-thread:*current-thread*)
      (let ((result (resume-fiber fiber)))
        (assert (= (first result) 1000))
        (assert (eq (second result) (fiber-heap fiber))))
      (release-fiber fiber))
    (sb-thread:join-thread owner)))

;;; --- Stress ---

(with-test (:name (:process-heap :stress :mixed-allocation-with-global-gc)
            :slow t)
  (with-fiber-thread ()
    (let ((fibers (loop for i below 8
                        collect (make-fiber
                                 (lambda ()
                                   (let ((table (make-hash-table :test 'equal))
                                         (strings nil))
                                     (dotimes (round 50)
                                       (dotimes (i 200)
                                         (setf (gethash (format nil "~D-~D" round i) table)
                                               (make-array (mod i 37) :initial-element i))
                                         (push (make-string 20 :initial-element #\s) strings))
                                       (when (> (length strings) 500) (setf strings nil))
                                       (yield-fiber (hash-table-count table)))
                                     (hash-table-count table)))
                                 :heap (make-heap :gc-threshold (* 200 1024))))))
      (loop while (some #'fiber-alive-p fibers)
            for round from 0
            do (dolist (f fibers)
                 (when (fiber-alive-p f) (resume-fiber f)))
               (when (zerop (mod round 10)) (sb-ext:gc)))
      (dolist (f fibers)
        (assert (> (heap-gc-count (fiber-heap f)) 0))
        (release-fiber f)))))

;;; Local collections on different threads run at the same time, weak
;;; objects and all.
(with-test (:name (:process-heap :threads :concurrent-collections))
  (let* ((nthreads 4)
         (threads
           (loop for t-index below nthreads
                 collect (sb-thread:make-thread
                          (lambda ()
                            (let ((h (make-heap :gc-threshold 0)))
                              (unwind-protect
                                   (with-heap (h)
                                     (let ((keep (make-array 64 :initial-element nil))
                                           (table (make-hash-table :test 'equal :weakness :value))
                                           (wps nil))
                                       (dotimes (round 40)
                                         (dotimes (i 2000)
                                           (let ((s (make-string 8 :initial-element
                                                                 (code-char (+ 65 (mod i 26))))))
                                             (setf (aref keep (mod i 64)) (list i s))
                                             (when (zerop (mod i 100))
                                               (setf (gethash (copy-seq s) table) (list i)))
                                             (when (zerop (mod i 500))
                                               (push (make-weak-pointer (list i)) wps))))
                                         (heap-gc h (zerop (mod round 5)))
                                         (loop for entry across keep
                                               when entry
                                                 do (destructuring-bind (i s) entry
                                                      (assert (= (length s) 8))
                                                      (assert (char= (char s 0)
                                                                     (code-char (+ 65 (mod i 26)))))))
                                         (maphash (lambda (k v)
                                                    (assert (stringp k))
                                                    (assert (listp v)))
                                                  table)
                                         (dolist (wp wps)
                                           (multiple-value-bind (v ok) (weak-pointer-value wp)
                                             (when ok (assert (listp v))))))
                                       (assert (null (verify-heap h)))
                                       (heap-gc-count h)))
                                (release-heap h))))
                          :name (format nil "concurrent-gc-~D" t-index)))))
    (dolist (th threads)
      (assert (> (sb-thread:join-thread th) 0)))
    (assert (>= (local-gc-concurrency-peak) 2))))

;;; Sends racing with the receiver's collections and with its exit.
(with-test (:name (:process-heap :threads :send-race-with-collections))
  (let* ((h (make-heap :gc-threshold 0))
         (stop nil)
         (senders
           (loop for k below 3
                 collect (let ((k k))
                           (sb-thread:make-thread
                            (lambda ()
                              (let ((mine (make-heap)))
                                (unwind-protect
                                     (with-heap (mine)
                                       (handler-case
                                           (loop until stop
                                                 do (send-message h (list k (make-string 16 :initial-element #\s)))
                                                 finally (return :stopped))
                                         (dead-heap-error () :dead)))
                                  (release-heap mine))))
                            :name (format nil "sender-~D" k))))))
    ;; As in WITH-TEST-HEAP: a failure must not be recorded in H.
    (handler-bind ((error (lambda (c)
                            (declare (ignore c))
                            (sb-fiber::%switch-heap 0))))
      (with-heap (h)
        (let ((received 0))
          (loop while (< received 3000)
                do (multiple-value-bind (msg found) (receive-message)
                     (cond (found
                            (assert (<= 0 (first msg) 2))
                            (assert (= (length (second msg)) 16))
                            (assert (every (lambda (c) (char= c #\s)) (second msg)))
                            (incf received)
                            (when (zerop (mod received 100))
                              (heap-gc h (zerop (mod received 1000)))))
                           (t (sb-thread:thread-yield)))))
          (assert (null (verify-heap h))))))
    ;; Exit while the senders are still going: they get DEAD-HEAP-ERROR
    ;; or see the stop flag, and the queued messages go with the heap.
    (release-heap h)
    (setf stop t)
    (dolist (th senders)
      (assert (member (sb-thread:join-thread th) '(:dead :stopped))))))

;;; --- A global reference into a released heap ---

;;; The store barrier covers stores into objects that already exist, so
;;; a closure built in the global heap over a process object escapes
;;; unseen: a closure captures its values when it is built.  Once the
;;; heap is released its pages are free, and following the stale pointer
;;; means tracing freed memory.  A global collection has to treat it as
;;; a leaf, and report it where ownership is being checked.

(defvar *stale-global-reference* (list nil))

(with-test (:name (:process-heap :released :stale-global-reference-is-a-leaf))
  (let ((h (make-heap :check-stores nil)))
    (with-heap (h)
      ;; Large, so the heap owns pages of its own that a small global
      ;; allocation will not take back before the collection below.
      (setf (car *stale-global-reference*)
            (make-array (* 64 1024) :initial-element 7)))
    (assert (eq (object-heap (car *stale-global-reference*)) h))
    (release-heap h)
    ;; The collection must survive the stale pointer, and name it.
    (let ((violations (verify-all-heaps)))
      (assert (member *stale-global-reference* violations :key #'first)))
    (setf (car *stale-global-reference*) nil)
    (assert (not (member *stale-global-reference* (verify-all-heaps)
                         :key #'first)))))

;;; --- Strict heaps and the runtime's own bookkeeping on exit paths ---

;;; A fiber's exit records its outcome in the fiber structure, which is
;;; global, and a strict heap counts any store into a global object as a
;;; violation.  The recording therefore runs with store checking
;;; suspended.  What each exit path must still deliver is the outcome
;;; itself -- the value, the condition, or the throw -- rather than a
;;; store error raised while recording it, and checking must be back on
;;; afterwards.

(defvar *strict-exit-target* (list :unchanged))

(defun make-strict-fiber (function)
  (make-fiber function :heap (make-heap :strict t)))

;;; Checking is on for HEAP: a store into a global object still signals,
;;; and does not go through.
(defun assert-strict-checking (heap)
  (with-heap (heap)
    (assert-error (setf (car *strict-exit-target*) (list :written))
                  process-heap-store-error))
  (assert (equal *strict-exit-target* '(:unchanged))))

(with-test (:name (:process-heap :strict :normal-completion-delivers-value))
  (with-fiber-thread ()
    (let* ((f (make-strict-fiber (lambda () (list :done 1 2))))
           (h (fiber-heap f)))
      (assert (equal (resume-fiber f) '(:done 1 2)))
      (assert (not (fiber-alive-p f)))
      (assert-strict-checking h)
      (release-fiber f))))

(with-test (:name (:process-heap :strict :uncaught-condition-escapes))
  (with-fiber-thread ()
    (let* ((f (make-strict-fiber
               (lambda () (error "escaping ~A" (list :strict)))))
           (h (fiber-heap f)))
      (handler-case (progn (resume-fiber f) (error "no condition escaped"))
        (process-heap-store-error (c)
          (error "recording the escape signalled ~A" c))
        (simple-error (c)
          ;; The original condition, copied out of the heap that is
          ;; about to go away.
          (assert (search "escaping (STRICT)" (princ-to-string c)))
          (assert (null (object-heap c)))))
      (assert (not (fiber-alive-p f)))
      (assert-strict-checking h)
      (release-fiber f))))

(with-test (:name (:process-heap :strict :throw-escapes-root-frame))
  (with-fiber-thread ()
    (let* ((f (make-strict-fiber
               (lambda ()
                 (throw 'sb-thread::%return-from-thread (list :thrown :out)))))
           (h (fiber-heap f))
           (result
             (handler-case
                 (catch 'sb-thread::%return-from-thread
                   (resume-fiber f)
                   :no-throw)
               (process-heap-store-error (c)
                 (error "recording the throw signalled ~A" c)))))
      (assert (equal result '(:thrown :out)))
      ;; The thrown values are copies: the heap that held them is about
      ;; to be released.
      (assert (null (object-heap result)))
      (assert (not (fiber-alive-p f)))
      (assert-strict-checking h)
      (release-fiber f))))

;;; --- Draining the violation record while it is being written ---

;;; The record is written from collections and from the store barrier on
;;; any thread, with an atomic increment and no lock.  Reading it and
;;; then resetting it therefore drops whatever is noted in between: the
;;; read did not see it and the reset discards it.  TAKE-HEAP-VIOLATIONS
;;; does both in one step, so every note is accounted for by exactly one
;;; caller.

(defvar *violation-sink* (list :sink))
(defvar *draining* nil)

(defun note-violations (start n)
  "Note N escape violations: a store of this heap's object into a global
one, under a heap that records rather than signals."
  (let ((h (make-heap :check-stores :record)))
    (unwind-protect
         (with-heap (h)
           (let ((mine (list :owned)))
             (sb-thread:wait-on-semaphore start)
             (dotimes (i n)
               (sb-vm::check-process-heap-store *violation-sink* mine))))
      (release-heap h)))
  :noted)

(defun drain-violations ()
  "Drain in a loop until the noters are done, returning the total count
drained."
  (let ((total 0))
    (loop while *draining*
          do (incf total (nth-value 1 (take-heap-violations))))
    total))

(with-test (:name (:process-heap :violations :take-is-atomic))
  (let* ((nthreads 4)
         (per-thread 2000)
         (expected (* nthreads per-thread))
         (start (sb-thread:make-semaphore)))
    (reset-heap-violations)
    (setf *draining* t)
    (let* ((noters (loop for i below nthreads
                         collect (sb-thread:make-thread
                                  #'note-violations
                                  :arguments (list start per-thread)
                                  :name (format nil "noter-~D" i))))
           (drainer (sb-thread:make-thread #'drain-violations :name "drainer")))
      (sb-thread:signal-semaphore start nthreads)
      (dolist (th noters)
        (assert (eq (sb-thread:join-thread th) :noted)))
      (setf *draining* nil)
      ;; Everything noted is drained exactly once, however the drains
      ;; interleaved with the notes.
      (let ((total (+ (sb-thread:join-thread drainer)
                      (nth-value 1 (take-heap-violations)))))
        (assert (= total expected) ()
                "drained ~D violations, ~D were noted" total expected))
      (assert (zerop (nth-value 1 (take-heap-violations)))))))

(with-test (:name (:process-heap :violations :take-returns-details))
  (reset-heap-violations)
  (let ((h (make-heap :check-stores :record)))
    (unwind-protect
         (with-heap (h)
           (sb-vm::check-process-heap-store *violation-sink* (list :escapee)))
      (release-heap h))
    (multiple-value-bind (details total) (take-heap-violations)
      (assert (= total 1))
      (assert (= (length details) 1))
      (assert (eq (first (first details)) *violation-sink*))
      (assert (null (object-heap (first details)))))
    ;; Taking clears the record.
    (multiple-value-bind (details total) (take-heap-violations)
      (assert (null details))
      (assert (zerop total)))))
