;;;; Fiber integration, threading and stress tests for local heaps.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

(unless (and (member :sb-thread *features*)
             (member :sb-local-heaps *features*)
             (member :sb-fiber *features*))
  (invoke-restart 'run-tests::skip-file))

(require :sb-fiber)
(use-package :sb-fiber)

(defmacro with-test-heap ((var &rest args) &body body)
  `(let ((,var (make-heap ,@args)))
     (unwind-protect
          (handler-bind ((error (lambda (c)
                                  (declare (ignore c))
                                  (sb-fiber::%switch-heap 0))))
            ,@body)
       (release-heap ,var))))

;;; --- Fibers with heaps ---

(with-test (:name (:local-heap :fiber :allocates-in-own-heap))
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

(with-test (:name (:local-heap :fiber :explicit-heap))
  (with-fiber-thread ()
    (let* ((h (make-heap :name "explicit"))
           (f (make-fiber (lambda () (object-heap (list 1))) :heap h)))
      (assert (eq (fiber-heap f) h))
      (assert (eq (join-fiber f) h))
      (assert-error (make-fiber (lambda ()) :heap h))
      (release-fiber f)
      (assert (not (heap-alive-p h))))))

(with-test (:name (:local-heap :fiber :values-cross-boundary-by-copy))
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

(with-test (:name (:local-heap :fiber :symbol-function-designator))
  (with-fiber-thread ()
    (let ((f (make-fiber 'heap-worker-function :heap t)))
      (assert (equal (join-fiber f) '(:worker :done)))
      (release-fiber f))))

(with-test (:name (:local-heap :fiber :closure-from-process-rejected))
  (with-fiber-thread ()
    (let ((outer (make-fiber (lambda ()
                               (let ((n (list 1)))
                                 (handler-case
                                     (progn (make-fiber (lambda () n) :heap t) :created)
                                   (cross-heap-reference () :rejected))))
                             :heap t)))
      (assert (eq (join-fiber outer) :rejected))
      (release-fiber outer))))

(with-test (:name (:local-heap :fiber :escaping-condition-is-copied))
  (with-fiber-thread ()
    (let ((f (make-fiber (lambda ()
                           (error "boom ~A" (make-string 3 :initial-element #\!)))
                         :heap t)))
      (handler-case (progn (resume-fiber f) (error "no condition"))
        (simple-error (c)
          (assert (null (object-heap c)))
          (assert (search "boom !!!" (princ-to-string c)))))
      (release-fiber f))))

(with-test (:name (:local-heap :fiber :interrupt-with-local-condition))
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

(with-test (:name (:local-heap :fiber :auto-gc-inside-fiber))
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

(with-test (:name (:local-heap :fiber :many-fibers-many-heaps))
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

;;; RELEASE-FIBER with a local heap installed, strict or not, for fibers
;;; with and without a heap of their own, and for a fiber whose own heap
;;; is the one installed.  Releasing stored a SAP allocated in the
;;; installed heap into the global fiber wrapper, which the barrier
;;; refused.
(with-test (:name (:local-heap :release-fiber :with-a-heap-installed))
  (with-fiber-thread ()
    (dolist (strict '(nil t))
      (with-test-heap (h :strict strict)
        (dolist (own-heap '(nil t))
          (let ((w (make-fiber 'yield-fiber :heap own-heap)))
            (resume-fiber w)
            (with-heap (h)
              (release-fiber w)
              (assert (eq h (current-heap))))
            (assert (fiber-released-p w))))))
    (let* ((w (make-fiber 'yield-fiber :heap t))
           (own (fiber-heap w)))
      (resume-fiber w)
      (with-heap (own)
        (release-fiber w)
        (assert (null (current-heap))))
      (assert (fiber-released-p w))
      (assert (not (heap-alive-p own))))))

;;; A main fiber installed by WITH-CURRENT-FIBER runs with the heap
;;; installed on the thread, which a switch back to it restores.  The
;;; runtime does not follow heap changes for a fiber that is not
;;; current, so installing records the heap afresh.  The worker is made
;;; outside WITH-HEAP: MAKE-FIBER's own heap switch would record it.
(with-test (:name (:local-heap :with-current-fiber :switch-back-restores-heap))
  (let* ((host (make-main-fiber :current nil))
         (w (with-current-fiber (host)
              (make-fiber 'yield-fiber))))
    (with-test-heap (h)
      (with-heap (h)
        (with-current-fiber (host)
          (resume-fiber w)
          (assert (eq h (current-heap))))))
    (release-fiber w)
    (release-fiber host)))

;;; --- Copying structures and standard objects ---

(defstruct heap-test-struct a (b 0d0 :type double-float) (c 0 :type fixnum))

(with-test (:name (:local-heap :copy-for-transfer :structures-and-hash-tables))
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

(with-test (:name (:local-heap :copy-for-transfer :standard-objects))
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

(with-test (:name (:local-heap :fiber :ping-pong))
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

(with-test (:name (:local-heap :threads :independent-collections))
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

(with-test (:name (:local-heap :threads :cross-thread-send))
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

(with-test (:name (:local-heap :threads :heap-in-use-on-other-thread))
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

(with-test (:name (:local-heap :fiber :migrate-with-heap))
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

(with-test (:name (:local-heap :stress :mixed-allocation-with-global-gc)
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
(with-test (:name (:local-heap :threads :concurrent-collections))
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
(with-test (:name (:local-heap :threads :send-race-with-collections))
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

;;; --- Draining the violation record while it is being written ---

;;; The record is written from collections and from the store barrier on
;;; any thread, with an atomic increment and no lock.  Reading it and
;;; then resetting it therefore drops whatever is noted in between: the
;;; read did not see it and the reset discards it.  TAKE-HEAP-VIOLATIONS
;;; does both in one step, so every note is accounted for by exactly one
;;; caller.

(defvar *violation-sink* (list :sink))
(defvar *draining* nil)

;;; A recorded escape is left in place, so the sink points into the heap
;;; until it is cleared; a later collection would otherwise trace it into
;;; pages another heap has reused.
(defun clear-violation-sink ()
  (setf (car *violation-sink*) :sink))

(defun note-violations (start n)
  "Note N escape violations: a store of this heap's object into a global
one, under a heap that records rather than signals."
  (let ((h (make-heap :check-stores :record)))
    (unwind-protect
         (with-heap (h)
           (let ((mine (list :owned)))
             (sb-thread:wait-on-semaphore start)
             (dotimes (i n)
               ;; An escaping store: this heap's object into a global cons.
               (setf (car *violation-sink*) mine))))
      (clear-violation-sink)
      (release-heap h)))
  :noted)

(defun drain-violations ()
  "Drain in a loop until the noters are done, returning the total count
drained."
  (let ((total 0))
    (loop while *draining*
          do (incf total (nth-value 1 (take-heap-violations))))
    total))

(with-test (:name (:local-heap :violations :take-is-atomic))
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

(with-test (:name (:local-heap :violations :take-returns-details))
  (reset-heap-violations)
  (let ((h (make-heap :check-stores :record)))
    (unwind-protect
         (with-heap (h)
           (setf (car *violation-sink*) (list :escapee)))
      (clear-violation-sink)
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

;;; Generic dispatch may update global caches, but effective methods must
;;; retain the caller's heap on first calls and subsequent cache misses.
(defgeneric strict-dispatch-owner (value))
(defmethod strict-dispatch-owner ((value integer))
  (sb-vm::object-owner (list value)))
(defmethod strict-dispatch-owner ((value string))
  (sb-vm::object-owner (list value)))
(defmethod strict-dispatch-owner ((value cons))
  (sb-vm::object-owner (list value)))

(with-test (:name (:local-heap :strict :generic-dispatch-allocation))
  (with-test-heap (heap :check-stores :error :strict t)
    (dolist (value '(1 2 "new specializer" "warm" (third) (warm)))
      (let ((owner (with-heap (heap) (strict-dispatch-owner value))))
        (assert (= owner (sb-fiber::heap-id heap)))))))

(defvar *strict-dispatch-target* (make-hash-table))
(defgeneric strict-dispatch-store (value))
(defmethod strict-dispatch-store ((value integer))
  (setf (gethash :escaped *strict-dispatch-target*) (list value)))
(defmethod strict-dispatch-store ((value string))
  (setf (gethash :escaped *strict-dispatch-target*) (list value)))

(with-test (:name (:local-heap :strict :generic-method-store-rejected))
  (with-test-heap (heap :check-stores :error :strict t)
    (dolist (value '(1 2 "new specializer" "warm"))
      (assert
       (handler-case
           (with-heap (heap) (strict-dispatch-store value) nil)
         (heap-store-error () t)))
      (assert (zerop (hash-table-count *strict-dispatch-target*))))))

;;; ALLOCATE-INSTANCE of a constant class calls an allocator CTOR whose
;;; first call compiles the optimized allocator and records the CTOR on
;;; the class. Made inside a checked heap, both run in the global heap.
(defclass checked-allocator-probe () ((x :initform nil)))
(defun checked-allocator-probe ()
  (allocate-instance (find-class 'checked-allocator-probe)))

(with-test (:name (:local-heap :checked :first-optimized-allocator))
  (with-test-heap (heap :check-stores :error)
    (let ((owner (with-heap (heap)
                   (sb-vm::object-owner (checked-allocator-probe)))))
      (assert (= owner (sb-fiber::heap-id heap)))))
  (gc :full t)
  (assert (typep (checked-allocator-probe) 'checked-allocator-probe)))

;;; The format parsers that run at interpretation time cons in the system
;;; TLAB; a result mixing those global conses with the caller's SUBSEQs and
;;; directives would point the global heap into the local one.
(with-test (:name (:local-heap :checked :format-parsers-build-globally))
  (with-test-heap (heap :check-stores :error)
    ;; A condition report's control is a global FMT-CONTROL whose parse is
    ;; memoized into it on first use: take the type error's and forget its
    ;; parse, so the first use is made from the local heap.
    (let ((control (find-if (lambda (x)
                              (and (typep x 'sb-format::fmt-control)
                                   (search "~@<Value of ~S in ~_~A ~I~_is"
                                           (sb-format::fmt-control-string x))))
                            (sb-vm:list-allocated-objects
                             :all :type sb-vm:funcallable-instance-widetag))))
      (assert control)
      (setf (sb-format::fmt-control-memo control) nil)
      (with-heap (heap)
        (format nil (copy-seq "~@<Value of ~S in ~_~A is ~_not a ~S.~:@>") 1 "x" 'y)
        (format nil (copy-seq "~10<~A~;~A~>") "ab" "cd")
        (format nil (copy-seq "~[zero~;one~:;many~]") 1)
        (format nil control 'v "ctx" 1 'string)
        nil))
    ;; While the heap is alive, nothing global may point into it.
    (assert (null (verify-all-heaps)))
    ;; Whether a garbage escape shows there depends on what the collection
    ;; finds reachable, so the parsers' results are checked directly: every
    ;; cons, element and directive's control string is global.
    (flet ((global-p (x) (or (sb-int:fixnump x) (zerop (sb-vm::object-owner x)))))
      (with-heap (heap)
        (let* ((string (copy-seq "~@<Value of ~S in ~_~A is ~_not a ~S.~:@>"))
               (tokens (sb-format::tokenize-control-string string))
               (insides (nth-value 2 (multiple-value-bind (segments first-semi close)
                                         (sb-format::parse-format-justification (cdr tokens))
                                       (sb-format::parse-format-logical-block
                                        segments nil first-semi close nil string 0)))))
          (dolist (list (list tokens insides))
            (loop for cell on list
                  for x = (car cell)
                  do (assert (global-p cell))
                     (assert (global-p x))
                     (when (sb-format::format-directive-p x)
                       (assert (global-p (sb-format::directive-string x))))))))))
  (gc :full t))

;;; A frame walk from a checked heap, over an interrupted frame, makes
;;; global frames that refer only to global objects, and fills the debug
;;; caches without a refused store.
(defun frame-walk-probe (x) (length x))
(declaim (notinline frame-walk-probe))

(with-test (:name (:local-heap :checked :frame-walk-builds-globally))
  (let ((frames 0) (bad 0) (refused nil))
    (flet ((global-p (x) (or (null x) (sb-int:fixnump x)
                             (zerop (sb-vm::object-owner x)))))
      (with-test-heap (heap :check-stores :error)
        (with-heap (heap)
          (handler-case
              (handler-bind
                  ((type-error
                     (lambda (c)
                       (declare (ignore c))
                       (sb-debug:map-backtrace
                        (lambda (frame)
                          (incf frames)
                          (unless (and (global-p frame)
                                       (or (not (sb-di::compiled-frame-p frame))
                                           (global-p (sb-di::compiled-frame-escaped frame))))
                            (incf bad)))))))
                (frame-walk-probe (make-hash-table)))
            (heap-store-error () (setf refused t))
            (type-error () nil))
          nil)))
    (assert (plusp frames))
    (assert (not refused))
    (assert (zerop bad))))

(defun strict-cached-package () (find-package "SB-FIBER"))
(with-test (:name (:local-heap :strict :cold-package-cache))
  (with-test-heap (heap :check-stores :error :strict t)
    (assert (eq (with-heap (heap) (strict-cached-package))
                (find-package "SB-FIBER")))))

;;; The runtime's own stores on a thread's behalf are not the program's,
;;; and a strict heap does not refuse them: printing a condition report
;;; tokenizes its control string, interning a pathname fills the pathname
;;; tables, and a contended mutex marks the thread as waiting.
(with-test (:name (:local-heap :strict :condition-report-tokenizes))
  (with-test-heap (heap :check-stores :error :strict t)
    (let ((report (with-heap (heap)
                    (handler-case (error "Strict tokenizer ~S probe ~D" :x 1)
                      (simple-error (e) (princ-to-string e))))))
      (assert (search "Strict tokenizer :X probe 1" report)))))

(with-test (:name (:local-heap :strict :pathname-interning))
  (with-test-heap (heap :check-stores :error :strict t)
    (let ((name (format nil "/strict-heap-~D/probe.txt" (random 1000000000))))
      (with-heap (heap)
        (assert (equal "probe" (pathname-name (pathname name))))
        (assert (probe-file "/"))))))

(with-test (:name (:local-heap :strict :contended-mutex-wait-mark))
  (let* ((mutex (sb-thread:make-mutex))
         (held (sb-thread:make-semaphore))
         (holder (sb-thread:make-thread
                  (lambda ()
                    (sb-thread:with-mutex (mutex)
                      (sb-thread:signal-semaphore held)
                      (sleep 0.3))))))
    (sb-thread:wait-on-semaphore held)
    (with-test-heap (heap :check-stores :error :strict t)
      (assert (eq :held (with-heap (heap)
                          (sb-thread:with-mutex (mutex) :held)))))
    (sb-thread:join-thread holder)))

;;; A thread's interruption queue is the runtime's bookkeeping on the thread
;;; struct: delivering an interruption to a thread inside a strict heap pops
;;; it, and interrupting from inside one appends to the target's queue.
(with-test (:name (:local-heap :strict :interruption-delivered))
  (let* ((ready (sb-thread:make-semaphore))
         (worker (sb-thread:make-thread
                  (lambda ()
                    (with-test-heap (heap :check-stores :error :strict t)
                      (with-heap (heap)
                        (sb-thread:signal-semaphore ready)
                        (sleep 0.5)))
                    :done))))
    (sb-thread:wait-on-semaphore ready)
    ;; The worker only returns :DONE if the interruption did not signal.
    (sb-thread:interrupt-thread worker (lambda () nil))
    (assert (eq :done (sb-thread:join-thread worker)))))

(with-test (:name (:local-heap :strict :interruption-sent))
  (let* ((stop (sb-thread:make-semaphore))
         (ran (sb-thread:make-semaphore))
         (target (sb-thread:make-thread
                  (lambda () (sb-thread:wait-on-semaphore stop))))
         (action (lambda () (sb-thread:signal-semaphore ran))))
    (with-test-heap (heap :check-stores :error :strict t)
      (with-heap (heap)
        (sb-thread:interrupt-thread target action)))
    (assert (sb-thread:wait-on-semaphore ran :timeout 5))
    (sb-thread:signal-semaphore stop)
    (sb-thread:join-thread target)))

;;; A timer's signal can arrive on a thread with a strict heap installed; the
;;; schedule it updates and the timer it runs are global.
(with-test (:name (:local-heap :strict :timer-expires))
  (with-test-heap (heap :check-stores :error :strict t)
    (let ((timer (sb-kernel::with-global-heap
                   (sb-ext:make-timer (lambda () (sb-impl::timeout-cerror))
                                      :thread sb-thread:*current-thread*))))
      (assert (eq :timed-out
                  (with-heap (heap)
                    (handler-case
                        (progn
                          (sb-kernel::with-global-heap (sb-ext:schedule-timer timer 0.2))
                          (sleep 3)
                          :finished)
                      (sb-ext:timeout () :timed-out)))))
      (sb-ext:unschedule-timer timer))))

;;; A frame can still hold an object of a heap it has released. A global
;;; collection that finds that stale root must not trace the memory the
;;; release gave back.
(defstruct stale-root-record message fields)

(defun collect-after-release-in-same-frame (n)
  (let ((retained '()))
    (dotimes (i n)
      (let ((heap (make-heap :check-stores :error :strict t))
            (record nil))
        (unwind-protect
             (with-heap (heap)
               (setf record (make-stale-root-record
                             :message (format nil "request ~D" i)
                             :fields (list (copy-seq "a") (vector 1 2 3))))
               (without-heap (push (globalize record) retained)))
          (release-heap heap))
        (sb-ext:gc :full t)))
    retained))

(with-test (:name (:local-heap :full-gc-after-release-with-stale-root))
  (let ((retained (collect-after-release-in-same-frame 50)))
    (assert (= 50 (length retained)))
    (assert (every (lambda (r) (search "request" (stale-root-record-message r)))
                   retained))))

;;; A thread-local symbol value is stored without the store barrier, so
;;; it can outlive the heap that owned it.  Collections that scan the
;;; thread afterwards must not mark through it into memory that is free,
;;; or that a later heap took.

(defvar *thread-local-value* nil)

(defun churn-heaps (n bytes)
  "Claim and release pages the way short-lived connection heaps do."
  (dotimes (i n)
    (let ((heap (make-heap)))
      (with-heap (heap)
        (make-list (floor bytes 16))
        (make-array (floor bytes 8)))
      (release-heap heap))))

(with-test (:name (:local-heap :thread-local-value-outlives-heap :global-gc))
  (dotimes (i 20)
    (let ((*thread-local-value* nil))
      (let ((heap (make-heap)))
        (with-heap (heap)
          (setq *thread-local-value*
                (list (make-list 200) (make-array 100000))))
        (release-heap heap))
      (sb-ext:gc :full t)
      (churn-heaps 4 (* 256 1024))
      (sb-ext:gc))))

(with-test (:name (:local-heap :thread-local-value-outlives-heap :local-gc))
  (dotimes (i 20)
    (let ((*thread-local-value* nil))
      (let ((heap (make-heap)))
        (with-heap (heap)
          (dotimes (k 300)
            (setq *thread-local-value* (make-array 7 :initial-element k))))
        (release-heap heap))
      ;; A new heap on this thread takes the released blocks, so the
      ;; stale value points into the middle of its objects.
      (let ((heap (make-heap)))
        (with-heap (heap)
          (let ((keep (loop repeat 300
                            collect (make-array 13 :initial-element i))))
            (heap-gc heap t)
            (sb-ext:gc :full t)
            (heap-gc heap t)
            (assert (= 300 (length keep)))))
        (release-heap heap)))))

(defvar *dangling-holder* nil)

;;; A global object built from a heap's objects is not a store the
;;; barrier sees.  Once the heap is released the edge dangles, and a
;;; global collection must not mark through it.
(with-test (:name (:local-heap :global-object-with-edge-into-released-heap))
  (dotimes (i 20)
    (let ((heap (make-heap))
          (local nil))
      (with-heap (heap)
        (setq local (list (make-list 300) (make-array 100000))))
      (setq *dangling-holder* (cons local nil))
      (release-heap heap))
    (sb-ext:gc :full t)
    (churn-heaps 4 (* 256 1024))
    (sb-ext:gc :full t)
    (setq *dangling-holder* nil)))

;;; The same through a cons's cdr, which tracing follows without the
;;; ownership checks a car goes through.
(with-test (:name (:local-heap :global-cons-with-cdr-into-released-heap))
  (dotimes (i 20)
    (let ((heap (make-heap))
          (vector nil)
          (list nil))
      (with-heap (heap)
        (setq vector (make-array 100000 :initial-element i)
              list (make-list 300 :initial-element i)))
      (setq *dangling-holder* (list* :vector (list* :list list) vector))
      (release-heap heap))
    (sb-ext:gc :full t)
    (churn-heaps 4 (* 256 1024))
    (sb-ext:gc :full t)
    (setq *dangling-holder* nil)))

(with-test (:name (:local-heap :global-cons-with-cdr-into-heap :is-a-violation))
  (let ((heap (make-heap)))
    (unwind-protect
         (let ((local nil))
           (with-heap (heap) (setq local (make-array 10)))
           (setq *dangling-holder* (cons nil local))
           (let ((violations (verify-all-heaps)))
             (assert (find *dangling-holder* violations
                           :key #'first))))
      (setq *dangling-holder* nil)
      (release-heap heap))))

;;; Reading a condition slot caches it on the condition's assigned-slots
;;; list.  The cells belong with the condition: a global cell holding a
;;; heap's value is one the heap's collector does not trace, and one the
;;; global collector reaches into the heap from.
(defun condition-cells-owned-by (condition owner)
  (loop for tail on (sb-kernel::condition-assigned-slots condition)
        always (and (= owner (sb-vm::object-owner tail))
                    (= owner (sb-vm::object-owner (car tail))))))

(with-test (:name (:local-heap :checked :condition-slot-cells-stay-local))
  (with-test-heap (heap :check-stores :error)
    (with-heap (heap)
      (let* ((datum (list 1 2 3))
             (c (make-condition 'type-error :datum datum :expected-type 'string))
             (owner (sb-vm::object-owner c)))
        (assert (/= 0 owner))
        (assert (eq datum (type-error-datum c)))
        (assert (eq 'string (without-heap (type-error-expected-type c))))
        (assert (sb-kernel::condition-assigned-slots c))
        (assert (condition-cells-owned-by c owner))))))

;;; GLOBALIZE fills its table of copies in the global heap.  A vector
;;; the table outgrows is garbage that a conservative root can retain,
;;; so no entry may refer to the heap being copied from.
(with-test (:name (:local-heap :globalize :table-holds-no-source-objects))
  (let ((table (make-hash-table :test 'eql)))
    (with-test-heap (heap)
      (with-heap (heap)
        (let ((local (loop for i below 200 collect (list i (format nil "~D" i)))))
          (without-heap (sb-fiber::%copy-object local table)))))
    (assert (> (hash-table-count table) 200))
    (loop for key being the hash-keys of table using (hash-value copy)
          do (assert (typep key 'fixnum))
             (assert (zerop (sb-vm::object-owner copy))))))

(defparameter *heap-reader-long-token* (make-string 300 :initial-element #\7))

(defun check-heap-reads (values)
  (destructuring-bind (d i r car token-buf kw list long symbol) values
    (assert (eql d 0.5d0))
    (assert (eql i -42))
    (assert (eql r 1/3))
    (assert (eq car 'car))
    (assert (eq token-buf 'sb-impl::token-buf))
    (assert (eq kw :test))
    (assert (equal list '(1 cdr "text")))
    (assert (= long (parse-integer *heap-reader-long-token*)))
    (assert (string= (symbol-name symbol) *heap-reader-long-token*))
    (assert (eq (symbol-package symbol) (find-package "CL-USER")))))

(defun read-in-heap (heap)
  "Read numbers, symbols, a list and two long tokens inside HEAP, and
   check them there: the values are owned by HEAP."
  (with-heap (heap)
    (let ((values (list (read-from-string "0.5d0")
                        (read-from-string "-42")
                        (read-from-string "1/3")
                        (read-from-string "cl:car")
                        (read-from-string "sb-impl::token-buf")
                        (read-from-string ":test")
                        (read-from-string "(1 cl:cdr \"text\")")
                        (read-from-string *heap-reader-long-token*)
                        (read-from-string
                         (concatenate 'string "cl-user::|"
                                      *heap-reader-long-token* "|")))))
      (check-heap-reads values)
      t)))

(with-test (:name (:local-heap :strict :reader-token-buffers))
  (read-from-string "(warm cl:car)")
  (intern *heap-reader-long-token* "CL-USER")
  (dotimes (i 3)
    (with-test-heap (heap :check-stores :error :strict t)
      (assert (read-in-heap heap))))
  (dotimes (i 2)
    (with-test-heap (heap :check-stores :error)
      (assert (read-in-heap heap))))
  ;; No heap-owned buffer reached the pool.
  (loop for buffer = sb-impl::*token-buf-pool*
          then (sb-impl::token-buf-next buffer)
        while buffer
        do (assert (not (sb-vm::locally-owned-p buffer)))
           (assert (sb-impl::token-buf-pooled buffer)))
  (assert (equal (read-from-string "(cl:car 1.5d0)") '(car 1.5d0))))

;;; --- Allocation trap ---

(defun churn (bytes &optional (chunk 1024))
  (declare (type fixnum bytes chunk))
  (let ((last nil))
    (loop repeat (ceiling bytes chunk)
          do (setq last (make-array (floor chunk sb-vm:n-word-bytes))))
    last))

(defvar *traps* nil)

(defmacro counting-traps (&body body)
  `(let ((*traps* 0))
     (handler-bind ((heap-allocation-trap
                      (lambda (c) (declare (ignore c)) (incf *traps*))))
       ,@body)))

(with-test (:name (:local-heap :allocation-trap :fires-once))
  (with-test-heap (h :check-stores nil)
    (let ((condition nil)
          (trap nil))
      (with-heap (h)
        (setf trap (arm-heap-allocation-trap h (* 64 1024)))
        (assert (= trap (heap-allocation-trap-threshold h)))
        (counting-traps
          (handler-bind ((heap-allocation-trap
                           (lambda (c) (unless condition (setf condition c)))))
            (churn (* 1024 1024)))
          (assert (= 1 *traps*))))
      (assert condition)
      (assert (eq h (heap-allocation-trap-heap condition)))
      (assert (= trap (heap-allocation-trap-trap condition)))
      ;; Delivered at the end of the claim that crossed the threshold.
      (assert (< trap (heap-allocation-trap-claimed condition)
                 (+ trap sb-vm:gencgc-page-bytes 1)))
      (assert (null (heap-allocation-trap-threshold h))))))

(with-test (:name (:local-heap :allocation-trap :is-a-plain-condition))
  (assert (subtypep 'heap-allocation-trap 'condition))
  (assert (not (subtypep 'heap-allocation-trap 'error)))
  (assert (not (subtypep 'heap-allocation-trap 'storage-condition))))

(with-test (:name (:local-heap :allocation-trap :rearm-and-disarm))
  (with-test-heap (h)
    (with-heap (h)
      (counting-traps
        (arm-heap-allocation-trap h (* 64 1024))
        (disarm-heap-allocation-trap h)
        (assert (null (heap-allocation-trap-threshold h)))
        (churn (* 1024 1024))
        (assert (= 0 *traps*))
        ;; Re-arming replaces the threshold and counts from now.
        (arm-heap-allocation-trap h (* 16 1024 1024))
        (arm-heap-allocation-trap h (* 64 1024))
        (churn (* 1024 1024))
        (assert (= 1 *traps*))
        (arm-heap-allocation-trap h (* 64 1024))
        (churn (* 1024 1024))
        (assert (= 2 *traps*))))))

(with-test (:name (:local-heap :allocation-trap :claimed-is-monotonic-across-gc))
  (with-test-heap (h :gc-threshold (* 256 1024))
    (with-heap (h)
      (let ((claimed (heap-bytes-claimed h))
            (peak 0)
            (fell nil))
        (counting-traps
          (arm-heap-allocation-trap h (* 8 1024 1024))
          (dotimes (i 64)
            (churn (* 256 1024))
            (let ((now (heap-bytes-claimed h))
                  (footprint (heap-bytes-allocated h)))
              (assert (>= now claimed))
              (when (< footprint peak) (setf fell t))
              (setf peak (max peak footprint)
                    claimed now)))
          (assert (plusp (heap-gc-count h)))
          (assert fell)
          (assert (< peak (* 4 1024 1024)))
          (assert (>= claimed (* 16 1024 1024)))
          (assert (= 1 *traps*)))))))

(with-test (:name (:local-heap :allocation-trap :deferred-by-without-interrupts))
  (with-test-heap (h)
    (with-heap (h)
      (counting-traps
        (arm-heap-allocation-trap h (* 64 1024))
        (let ((inside -1))
          (sb-sys:without-interrupts
            (churn (* 1024 1024))
            (setf inside *traps*))
          (assert (= 0 inside))
          (assert (= 1 *traps*)))))))

(with-test (:name (:local-heap :allocation-trap :deferred-by-without-gcing))
  (with-test-heap (h)
    (with-heap (h)
      (counting-traps
        (arm-heap-allocation-trap h (* 64 1024))
        (let ((inside -1))
          (sb-sys:without-gcing
            (churn (* 256 1024))
            (setf inside *traps*))
          (assert (= 0 inside))
          (assert (= 1 *traps*)))))))

(with-test (:name (:local-heap :allocation-trap :disarm-withdraws-a-deferred-trap))
  (with-test-heap (h)
    (with-heap (h)
      (counting-traps
        (arm-heap-allocation-trap h (* 64 1024))
        (sb-sys:without-interrupts
          (churn (* 1024 1024))
          (disarm-heap-allocation-trap h))
        (churn (* 1024 1024))
        (assert (= 0 *traps*))))))

(with-test (:name (:local-heap :allocation-trap :waits-for-its-heap))
  (with-test-heap (h)
    (counting-traps
      (with-heap (h)
        (arm-heap-allocation-trap h (* 64 1024))
        (sb-sys:without-interrupts
          (churn (* 1024 1024))
          (sb-fiber::%switch-heap 0))
        (assert (= 0 *traps*))
        (sb-fiber::%switch-heap (sb-sys:sap-int (sb-fiber::heap-sap h)))
        (churn (* 64 1024))
        (assert (= 1 *traps*))))))

(define-alien-routine qsort void
  (base system-area-pointer)
  (nmemb unsigned-long)
  (size unsigned-long)
  (compar (function int (* double) (* double))))

(define-alien-callable churning-double-cmp int ((a (* double)) (b (* double)))
  (churn (* 16 1024))
  (let ((x (deref a)) (y (deref b)))
    (cond ((= x y) 0) ((< x y) -1) (t 1))))

(with-test (:name (:local-heap :allocation-trap :in-a-foreign-callback))
  (with-test-heap (h :check-stores nil)
    (let* ((vector (coerce (loop for i below 64 collect (float (mod (* i 37) 64) 1d0))
                           '(vector double-float)))
           (sorted (sort (copy-seq vector) #'<))
           (traps 0))
      (with-heap (h)
        (counting-traps
          (arm-heap-allocation-trap h (* 64 1024))
          (sb-sys:with-pinned-objects (vector)
            (qsort (sb-sys:vector-sap vector) (length vector)
                   (alien-size double :bytes)
                   (alien-callable-function 'churning-double-cmp)))
          (setf traps *traps*)))
      (assert (= 1 traps))
      (assert (equalp vector sorted)))))

(with-test (:name (:local-heap :allocation-trap :handler-unwinds))
  (with-test-heap (h)
    (with-heap (h)
      (dotimes (i 3)
        (arm-heap-allocation-trap h (* 64 1024))
        (assert (eq :refused
                    (block request
                      (handler-bind ((heap-allocation-trap
                                       (lambda (c)
                                         (declare (ignore c))
                                         (return-from request :refused))))
                        (churn (* 16 1024 1024))
                        :finished))))
        (assert (eq h (current-heap)))
        (assert (vectorp (churn (* 64 1024))))))))

(with-test (:name (:local-heap :allocation-trap :in-a-fiber))
  (with-fiber-thread ()
    (let* ((f (make-fiber (lambda ()
                            (let ((heap (current-heap)))
                              (arm-heap-allocation-trap heap (* 64 1024))
                              (handler-case (progn (churn (* 4 1024 1024)) :finished)
                                (heap-allocation-trap (c)
                                  (eq heap (heap-allocation-trap-heap c))))))
                          :heap t)))
      (assert (eq t (join-fiber f)))
      (release-fiber f))))

(with-test (:name (:local-heap :hard-limit :setter))
  (with-test-heap (h :gc-threshold 0)
    (with-heap (h)
      (assert (= 0 (heap-hard-limit h)))
      (setf (heap-hard-limit h) (+ (heap-bytes-allocated h) (* 256 1024)))
      (let ((keep nil))
        (assert (eq :caught
                    (handler-case (loop (push (make-array 100) keep))
                      (local-heap-exhausted-error () :caught))))
        (setf keep nil))
      (setf (heap-hard-limit h) 0)
      (assert (= 0 (heap-hard-limit h)))
      (let ((keep nil))
        (dotimes (i 10000) (push (make-array 100) keep))
        (assert (= 10000 (length keep)))))))

(with-test (:name (:local-heap :allocation-trap :strict))
  (with-test-heap (h :check-stores :error :strict t)
    (with-heap (h)
      (arm-heap-allocation-trap h (* 64 1024))
      (assert (eq :trapped
                  (handler-case (progn (churn (* 4 1024 1024)) :finished)
                    (heap-allocation-trap () :trapped)))))))

(with-test (:name (:local-heap :allocation-trap :with-a-queued-interruption))
  (with-test-heap (h)
    (let ((ran 0))
      (declare (fixnum ran))
      (with-heap (h)
        (arm-heap-allocation-trap h (* 64 1024))
        (assert (eq :refused
                    (block request
                      (handler-bind ((heap-allocation-trap
                                       (lambda (c)
                                         (declare (ignore c))
                                         (return-from request :refused))))
                        (sb-sys:without-interrupts
                          (sb-thread:interrupt-thread sb-thread:*current-thread*
                                                      (lambda () (incf ran)))
                          (churn (* 1024 1024)))
                        (churn (* 1024 1024))
                        :finished))))
        (loop repeat 100 until (= ran 1) do (sleep 0.01)))
      (assert (= 1 ran)))))

(define-condition inner-refusal (condition) ())

(with-test (:name (:local-heap :allocation-trap :function-signals-inward))
  (with-test-heap (h)
    (with-heap (h)
      (arm-heap-allocation-trap h (* 64 1024))
      (let ((*heap-allocation-trap-function*
              (lambda (trap)
                (assert (typep trap 'heap-allocation-trap))
                (signal 'inner-refusal))))
        (assert (eq :inner
                    (block inner
                      (handler-bind ((inner-refusal
                                       (lambda (c)
                                         (declare (ignore c))
                                         (return-from inner :inner))))
                        (churn (* 4 1024 1024))
                        :finished))))))))
