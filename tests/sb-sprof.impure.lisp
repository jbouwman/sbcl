
;;; This exists outside of the unit test in sb-sprof so that you can execute
;;; it with parallel-exec specifying an arbitrarily huge --runs_per_test.
;;; It is uncharacteristically verbose in its output for my liking,
;;; but I need to try to see it behaving badly (if it does),
;;; and there's really no other way than to watch for bad output.

#+(or win32 sparc) (invoke-restart 'run-tests::skip-file)

(require :sb-sprof)

;;; silly examples

(defun test-0 (n &optional (depth 0))
  (declare (optimize (debug 3)))
  (when (< depth n)
    (dotimes (i n)
      (test-0 n (1+ depth))
      (test-0 n (1+ depth))))
  (values 'a 'b 'c))

(defun test ()
  (sb-sprof:with-profiling (:reset t :max-samples 1000 :report :graph)
    (test-0 6)))
(compile 'test-0)
(compile 'test)
(with-test (:name :with-profiling-return-value)
  (let ((answer
         ;; don't want to actually see the report
         (let ((*standard-output* (make-broadcast-stream)))
           (multiple-value-list (test)))))
    (assert (equal answer '(a b c)))  ))

(defun consalot ()
  (let ((junk '()))
    (loop repeat 10000 do
         (push (make-array 10) junk))
    junk))
(compile 'consalot)
(defun consing-test ()
  ;; This used to test that rapid consing didn't improperly interrupt pseudo-atomic.
  ;; But now that the profiling signal isn't deferrable, I don't really think
  ;; this tests anything.
  (sb-sprof:with-profiling (:reset t
                          ;; setitimer with small intervals
                          ;; is broken on FreeBSD 10.0
                          ;; And ARM targets are not fast in
                          ;; general, causing the profiling signal
                          ;; to be constantly delivered without
                          ;; making any progress.
                          #-(or freebsd arm) :sample-interval
                          #-(or freebsd arm) 0.0001
                          #+arm :sample-interval #+arm 0.1
                          :report :graph)
    (loop with target = (+ (get-universal-time) 2)
          while (< (get-universal-time) target)
          do (consalot))))
(compile 'consing-test)
(with-test (:name :sprof-consing-test)
  ;; again don't want to actually see the report
  (let ((*standard-output* (make-broadcast-stream)))
    (consing-test))
  ;; For debugging purposes, print output for visual inspection to see where
  ;; the allocation sequence gets hit.
  ;; It can be interrupted even inside pseudo-atomic now.
  (disassemble #'consalot :stream *error-output*))

(load "../contrib/sb-sprof/test.lisp")

(with-test (:name :sprof)
  (with-scratch-file (f "fasl")
    (setq sb-sprof-test::*compiler-input* "../contrib/sb-sprof/graph.lisp"
          sb-sprof-test::*compiler-output* f
          ;; It was supposed to be 100 before I decreased it.
          ;; surely more samples is better, right?
          sb-sprof-test::*sprof-loop-test-max-samples* 100)
    (sb-sprof-test:run-tests)))

;;; Sample tags: the handler copies the thread's SAMPLE-TAG into each sample,
;;; and HARVEST-TRACES hands it back per stack.

(defun spin-tagged (seconds)
  (let ((end (+ (get-internal-real-time)
                (round (* seconds internal-time-units-per-second))))
        (x 0))
    (declare (fixnum x))
    (loop while (< (get-internal-real-time) end)
          do (dotimes (i 1000) (setf x (logand (+ x i) #xffff))))
    x))
(defun spin-tagged-a (seconds) (1+ (spin-tagged seconds))) ; not a tail call
(defun spin-tagged-b (seconds) (1+ (spin-tagged seconds)))
(declaim (notinline spin-tagged spin-tagged-a spin-tagged-b))
(compile 'spin-tagged)
(compile 'spin-tagged-a)
(compile 'spin-tagged-b)

(defun frames-by-tag ()
  (let ((by-tag (make-hash-table)))
    (sb-sprof:harvest-traces
     (lambda (tag thread count frames)
       (declare (ignore thread))
       (assert (plusp count))
       (dolist (frame frames)
         (pushnew frame (gethash tag by-tag) :test #'equal))))
    by-tag))

(with-test (:name (:sprof :sample-tag :accessors))
  (assert (eql 0 (sb-thread:join-thread
                  (sb-thread:make-thread (lambda () (sb-sprof:sample-tag))))))
  (let ((old (sb-sprof:sample-tag)))
    (unwind-protect
         (progn
           (setf (sb-sprof:sample-tag) 42)
           (assert (eql 42 (sb-sprof:sample-tag)))
           (setf (sb-sprof:sample-tag) most-negative-fixnum)
           (assert (eql most-negative-fixnum (sb-sprof:sample-tag)))
           (assert-error (funcall (compile nil '(lambda (x) (setf (sb-sprof:sample-tag) x)))
                                  (list 1))))
      (setf (sb-sprof:sample-tag) old))))

(with-test (:name (:sprof :sample-tag :splits-samples)
            :skipped-on (not :sb-thread))
  (sb-sprof:reset)
  (sb-sprof:start-profiling :sample-interval 0.0005 :max-samples 100000
                            :threads (list sb-thread:*current-thread*))
  (unwind-protect
       (dotimes (i 4)
         (setf (sb-sprof:sample-tag) 1)
         (spin-tagged-a 0.05)
         (setf (sb-sprof:sample-tag) -2)
         (spin-tagged-b 0.05))
    (setf (sb-sprof:sample-tag) 0)
    (sb-sprof:stop-profiling))
  (let ((by-tag (frames-by-tag)))
    (assert (member 'spin-tagged-a (gethash 1 by-tag)))
    (assert (not (member 'spin-tagged-b (gethash 1 by-tag))))
    (assert (member 'spin-tagged-b (gethash -2 by-tag)))
    (assert (not (member 'spin-tagged-a (gethash -2 by-tag)))))
  ;; Harvesting took them.
  (assert (zerop (hash-table-count (frames-by-tag)))))

(with-test (:name (:sprof :sample-tag :harvest-while-sampling)
            :skipped-on (not :sb-thread))
  (sb-sprof:reset)
  (sb-sprof:start-profiling :sample-interval 0.0005 :max-samples 100000
                            :threads (list sb-thread:*current-thread*))
  (unwind-protect
       (progn
         (setf (sb-sprof:sample-tag) 3)
         (spin-tagged-a 0.1)
         (let ((first (frames-by-tag)))
           (assert (member 'spin-tagged-a (gethash 3 first))))
         (setf (sb-sprof:sample-tag) 4)
         (spin-tagged-b 0.1))
    (setf (sb-sprof:sample-tag) 0)
    (sb-sprof:stop-profiling))
  (let ((second (frames-by-tag)))
    (assert (member 'spin-tagged-b (gethash 4 second)))
    (assert (not (member 'spin-tagged-a (gethash 4 second))))
    (assert (not (member 'spin-tagged-a (gethash 3 second))))))
