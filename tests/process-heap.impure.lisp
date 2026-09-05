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
