;;;; Build heaps: local heaps that take every allocation of their thread,
;;;; code and system allocations included, and are never collected.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

(unless (and (member :sb-local-heaps *features*)
             (member :sb-fiber *features*))
  (invoke-restart 'run-tests::skip-file))

(require :sb-fiber)
(use-package :sb-fiber)

;;; Build heaps are never released: global tables refer into them.
(defmacro with-build-heap ((var) &body body)
  `(let ((,var (make-heap :kind :build :check-stores nil)))
     ,@body))

(defun page-of (object)
  (sb-sys:with-pinned-objects (object)
    (floor (- (sb-kernel:get-lisp-obj-address object) sb-vm:dynamic-space-start)
           sb-vm:gencgc-page-bytes)))

(with-test (:name (:build-heap :takes-code))
  (with-build-heap (h)
    (let ((f (with-heap (h) (compile nil '(lambda (x) (* x 3))))))
      (assert (eq (object-heap f) h))
      (assert (eq (object-heap (sb-kernel:fun-code-header f)) h))
      (assert (= (funcall f 7) 21)))))

(with-test (:name (:build-heap :takes-system-allocations))
  (with-build-heap (h)
    (with-heap (h)
      ;; Symbols come from the system TLAB.
      (assert (eq (object-heap (make-symbol "BUILT")) h))
      (let ((package (make-package (symbol-name (gensym "BUILD-HEAP-PACKAGE-")) :use nil)))
        (unwind-protect
             (let ((symbol (intern "INTERNED" package)))
               (assert (eq (object-heap symbol) h))
               (assert (eq (object-heap package) h)))
          (delete-package package))))))

(with-test (:name (:build-heap :takes-with-global-heap))
  (with-build-heap (h)
    (with-heap (h)
      (assert (eq (object-heap (sb-kernel::with-global-heap (list 1 2))) h)))
    ;; A process heap still gives WITH-GLOBAL-HEAP to the global heap.
    (let ((p (make-heap)))
      (unwind-protect
           (with-heap (p)
             (assert (null (object-heap (sb-kernel::with-global-heap (list 1 2))))))
        (release-heap p)))))

(with-test (:name (:build-heap :is-never-collected))
  (with-build-heap (h)
    (with-heap (h)
      (assert-error (heap-gc h)))))

(with-test (:name (:build-heap :is-never-released))
  (with-build-heap (h)
    (assert (eq (heap-kind h) :build))
    (assert-error (release-heap h))
    (assert (heap-alive-p h))
    (assert (eq (object-heap (with-heap (h) (list 1))) h))))

(with-test (:name (:build-heap :claims-whole-pages))
  (with-build-heap (h)
    (let ((objects (with-heap (h)
                     (loop repeat 1000 collect (make-array 10))))
          (p (make-heap)))
      (assert (zerop (mod (heap-bytes-allocated h) sb-vm:gencgc-page-bytes)))
      (unwind-protect
           (let ((pages (remove-duplicates (mapcar #'page-of objects)))
                 (others (with-heap (p) (loop repeat 1000 collect (make-array 10)))))
             (assert (null (intersection pages (mapcar #'page-of others)))))
        (release-heap p)))))

;;; Global objects reached only from a build heap survive a global
;;; collection, and so does everything in the heap.
(with-test (:name (:build-heap :global-gc :keeps-everything))
  (with-build-heap (h)
    (let* ((global (list :only-from-the-heap))
           (f (with-heap (h)
                (let ((g (compile nil `(lambda () ',global))))
                  (setq global nil)
                  (list g (make-array 3 :initial-element :kept))))))
      (sb-ext:gc :full t)
      (assert (equal (funcall (first f)) '(:only-from-the-heap)))
      (assert (equalp (second f) #(:kept :kept :kept))))))

(declaim (notinline backtrace-names))
(defun backtrace-names ()
  (mapcar #'car (sb-debug:list-backtrace :count 10)))

(with-test (:name (:build-heap :backtrace-through-its-code))
  (with-build-heap (h)
    (let ((f (with-heap (h)
               (compile 'build-heap-caller
                        '(lambda () (prog1 (backtrace-names) (values)))))))
      (assert (member 'build-heap-caller (funcall f))))))

;;; What a module's FASL leaves behind lands in the heap, and works after
;;; the heap is uninstalled and a global collection has run.
(with-test (:name (:build-heap :load-fasl))
  (with-scratch-file (source "lisp")
    (with-scratch-file (fasl "fasl")
      (with-open-file (s source :direction :output :if-exists :supersede)
        (write-string "(defpackage \"BUILD-HEAP-MODULE\" (:use \"CL\"))
(in-package \"BUILD-HEAP-MODULE\")
(defstruct point x y)
(defclass shape () ((name :initarg :name :reader shape-name)))
(defgeneric area (shape))
(defmethod area ((shape shape)) 0)
(defvar *table* (make-hash-table))
(defun remember (k v) (setf (gethash k *table*) v))
(defun recall (k) (gethash k *table*))
" s))
      (compile-file source :output-file fasl)
      ;; Compiling made the package in the global heap; loading makes it anew.
      (delete-package "BUILD-HEAP-MODULE")
      (with-build-heap (h)
        ;; The stream is the builder's, not the module's.
        (with-open-file (stream fasl :element-type '(unsigned-byte 8))
          (with-heap (h) (load stream)))
        (let ((point (find-symbol "MAKE-POINT" "BUILD-HEAP-MODULE"))
              (remember (find-symbol "REMEMBER" "BUILD-HEAP-MODULE"))
              (recall (find-symbol "RECALL" "BUILD-HEAP-MODULE"))
              (area (find-symbol "AREA" "BUILD-HEAP-MODULE"))
              (shape (find-symbol "SHAPE" "BUILD-HEAP-MODULE")))
          (assert (eq (object-heap (find-package "BUILD-HEAP-MODULE")) h))
          (assert (eq (object-heap point) h))
          (assert (eq (object-heap (sb-kernel:fun-code-header
                                    (sb-kernel:%fun-fun (fdefinition point))))
                      h))
          (sb-ext:gc :full t)
          (funcall remember :k (funcall point :x 1 :y 2))
          (assert (typep (funcall recall :k) (find-symbol "POINT" "BUILD-HEAP-MODULE")))
          (assert (eql 0 (funcall area (make-instance shape :name "s")))))))))
