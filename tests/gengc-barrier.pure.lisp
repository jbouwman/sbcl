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

#-soft-card-marks
(invoke-restart 'run-tests::skip-file)

(defun assert-barriers (potential actual fun &rest compile-args)
  (let* ((old-potential sb-vm::*store-barriers-potentially-emitted*)
         (old-actual sb-vm::*store-barriers-emitted*))
    (apply #'checked-compile fun compile-args)
    (assert (= potential (- sb-vm::*store-barriers-potentially-emitted* old-potential)))
    (assert (= actual (- sb-vm::*store-barriers-emitted* old-actual)))))

(with-test (:name :rplaca-union-types)
  (assert-barriers 1 0
                   `(lambda (x y)
                      (when (typep y '(or fixnum null))
                        (rplaca x y)))))

(with-test (:name :move-from-fixnum)
  (assert-barriers 1 0
                   `(lambda (a b)
                      (declare (fixnum a))
                      (setf (car b) (1+ a))))
  (assert-barriers 1 0
                   `(lambda (a b)
                      (declare (fixnum a))
                      (setf (car b) (- a)))))

(with-test (:name :old-slot-set-no-barrier)
  (assert-barriers 1 0
                   '(lambda (y)
                     (let ((x (cons 0 0)))
                       (setf (car x) y)
                       x))))

(with-test (:name :consequent)
  (assert-barriers 2 2
                   `(lambda (x m j)
                      (setf (car x) m)
                      (print x)
                      (setf (cdr x) j)))
  (assert-barriers 2 2
                   `(lambda (x m j)
                      (setf (car x) m)
                      (setf x *)
                      (setf (cdr x) j)))
  #+(or arm64 x86-64)
  (progn
    ;; These counters are updated by REQUIRE-GENGC-BARRIER-P before card
    ;; mark coalescing. Both pointer values still need classification so
    ;; that reusing a card mark cannot omit a local-heap store check.
    (assert-barriers 2 2
                     `(lambda (x m j)
                        (setf (car x) m)
                        (setf (cdr x) j)))
    (assert-barriers 2 1
                     `(lambda (x m j)
                        (declare (fixnum m))
                        (setf (car x) m)
                        (setf (cdr x) j)))
    #+nil
    (assert-barriers 1 1
                     `(lambda (x m j)
                        (setf (car x) m)
                        (setf (cdr x) (list j))))))

#+(or arm64 x86-64)
(with-test (:name :consecutive-store-card-marks)
  ;; Count calls to the code generator independently of the classification
  ;; counters above. A local-heap check can collect, so its slow path must
  ;; mark the card again after returning; the ordinary path marks it once.
  (let ((marks nil)
        (remarking nil))
    (sb-int:encapsulate
     'sb-vm::emit-gengc-barrier 'count-card-marks
     (lambda (function &rest args)
       (push remarking marks)
       (apply function args)))
    #+sb-local-heaps
    (sb-int:encapsulate
     'sb-vm::emit-local-heap-store-check 'count-card-marks
     (lambda (function object value temp &optional remark-card)
       (let ((previous remarking))
         (unwind-protect
              (progn
                (setf remarking (or remarking remark-card))
                (funcall function object value temp remark-card))
           (setf remarking previous)))))
    (unwind-protect
         (checked-compile '(lambda (x m j)
                             (setf (car x) m)
                             (setf (cdr x) j)))
      (sb-int:unencapsulate 'sb-vm::emit-gengc-barrier 'count-card-marks)
      #+sb-local-heaps
      (sb-int:unencapsulate 'sb-vm::emit-local-heap-store-check 'count-card-marks))
    (assert (equal (reverse marks) #+sb-local-heaps '(nil t)
                                   #-sb-local-heaps '(nil)))))

(with-test (:name :dx)
  (assert-barriers 1 0
                   `(lambda ()
                      (let ((vector (make-array 10)))
                        (declare (dynamic-extent vector))
                        (setf (aref vector 0) *))))
  (assert-barriers 1 0
                   `(lambda ()
                      (declare (optimize (debug 2)))
                      (let ((vector (make-array 10)))
                        (declare (dynamic-extent vector))
                        (setf (aref vector 0) *)))))
