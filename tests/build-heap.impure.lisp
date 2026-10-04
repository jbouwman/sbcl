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

;;; --- Records ---

;;; Run BODY with a checked build heap installed and a recorder bound, and
;;; return the recorder.
(defmacro recording ((&optional (heap (gensym "HEAP"))) &body body)
  (let ((recorder (gensym "RECORDER")))
    `(let* ((,heap (make-heap :kind :build :check-stores :error))
            (,recorder (make-fragment-recorder ,heap)))
       (with-fragment-recorder (,recorder)
         (with-heap (,heap) ,@body))
       ,recorder)))

(defun assert-all-accounted (recorder)
  (let ((unaccounted (fragment-unaccounted-escapes recorder)))
    (assert (null unaccounted) ()
            "Unaccounted escapes:~{~%  ~S~}"
            (mapcar (lambda (escape)
                      (destructuring-bind (object value pc) escape
                        (list (type-of object) (type-of value) (store-site pc))))
                    unaccounted))))

(defun records-of-kind (recorder kind)
  (remove kind (fragment-records recorder) :key #'fragment-record-kind :test-not #'eq))

(with-test (:name (:build-heap :records :intern-into-a-global-package))
  (let* ((recorder (recording () (intern "BUILD-HEAP-FRESH-KEYWORD" :keyword)))
         (record (first (records-of-kind recorder :intern))))
    (assert-all-accounted recorder)
    (assert record)
    (assert (eq (fragment-record-target record) (find-package :keyword)))
    (assert (plusp (fragment-record-escapes record)))))

(defvar *build-heap-global-value* nil)

(with-test (:name (:build-heap :records :global-value-set))
  (let* ((recorder (recording () (setq *build-heap-global-value* (list :built))))
         (record (first (records-of-kind recorder :set-value))))
    (assert-all-accounted recorder)
    (assert record)
    (assert (eq (fragment-record-target record) '*build-heap-global-value*))))

(defvar *build-heap-global-cons* (list :global))

(with-test (:name (:build-heap :records :unrecorded-store-is-unaccounted))
  (let ((recorder (recording () (setf (car *build-heap-global-cons*) (list :built)))))
    (destructuring-bind ((object value pc)) (fragment-unaccounted-escapes recorder)
      (assert (eq object *build-heap-global-cons*))
      (assert (equal value '(:built)))
      (assert (typep pc 'sb-ext:word)))))

(with-test (:name (:build-heap :records :type-caches-are-not-records))
  (let ((recorder (recording ()
                    (sb-kernel:specifier-type
                     '(or (integer 3 17) (member :build-heap-a :build-heap-b))))))
    (assert-all-accounted recorder)
    (assert (plusp (fragment-cache-escapes recorder)))))

;;; A module's definitions leave nothing unaccounted: each store into a
;;; global object belongs to a record or a cache.
(with-test (:name (:build-heap :records :module-is-accounted-for))
  (with-scratch-file (source "lisp")
    (with-scratch-file (fasl "fasl")
      (with-open-file (s source :direction :output :if-exists :supersede)
        (write-string "(defpackage \"BUILD-HEAP-RECORDED\" (:use \"CL\") (:nicknames \"BHR\"))
(in-package \"BUILD-HEAP-RECORDED\")
(defstruct point x y)
(defclass shape (standard-object) ((name :initarg :name :reader shape-name)))
(defclass circle (shape) ((radius :initarg :radius)))
(define-condition shape-error (error) ((shape :initarg :shape)))
(deftype small () '(integer 0 7))
(defgeneric area (shape))
(defmethod area ((shape circle)) (* pi (expt (slot-value shape 'radius) 2)))
(defmethod print-object ((point point) stream) (format stream \"#<point>\"))
(defmacro twice (x) `(progn ,x ,x))
(defvar *shapes* (make-hash-table))
(defun remember (k v) (setf (gethash k *shapes*) v))
(declaim (ftype (function (small) small) bump))
(defun bump (n) (min 7 (1+ n)))
(export '(point shape circle area))
" s))
      (compile-file source :output-file fasl)
      (delete-package "BUILD-HEAP-RECORDED")
      ;; The stream is the builder's; the load is the module's.
      (with-open-file (stream fasl :element-type '(unsigned-byte 8))
        (let ((recorder (recording () (load stream))))
          (assert-all-accounted recorder)
          (assert (find (find-package "BUILD-HEAP-RECORDED")
                        (records-of-kind recorder :register-package)
                        :key (lambda (record) (first (fragment-record-args record)))))
          (assert (find #'print-object (records-of-kind recorder :add-method)
                        :key #'fragment-record-target))
          (assert (= 7 (funcall (find-symbol "BUMP" "BHR") 6))))))))

;;; --- Sealing ---

(defun build-heap-sealed-hook () :global)
(defvar *build-heap-sealed-value* nil)

;;; A sealed fragment's objects are those of its heap that the recorder
;;; reaches, with each record's replayed value; what the load or anything
;;; else left in the heap beyond them is not part of it.
(with-test (:name (:build-heap :seal :members))
  (with-scratch-file (source "lisp")
    (with-scratch-file (fasl "fasl")
      (with-open-file (s source :direction :output :if-exists :supersede)
        (write-string "(defpackage \"BUILD-HEAP-SEALED\" (:use \"CL\"))
(in-package \"BUILD-HEAP-SEALED\")
(defstruct point x y)
(defun norm (p) (+ (abs (point-x p)) (abs (point-y p))))
(setf (fdefinition 'cl-user::build-heap-sealed-hook) #'norm)
(setq cl-user::*build-heap-sealed-value* (list :sealed))
(let ((temporary (make-list 100))) (length temporary))
" s))
      (compile-file source :output-file fasl)
      (delete-package "BUILD-HEAP-SEALED")
      (let* ((heap (make-heap :kind :build :check-stores :error))
             (recorder (make-fragment-recorder heap)))
        (with-open-file (stream fasl :element-type '(unsigned-byte 8))
          (with-fragment-recorder (recorder)
            (with-heap (heap) (load stream))))
        (assert-all-accounted recorder)
        (let ((stray (with-heap (heap) (list :stray)))
              (count (seal-fragment recorder))
              (members (make-hash-table :test 'eq)))
          (map-fragment-members (lambda (x) (setf (gethash x members) t)) recorder)
          (assert (= count (hash-table-count members)))
          (flet ((member-p (x) (gethash x members)))
            (let ((norm (fdefinition (find-symbol "NORM" "BUILD-HEAP-SEALED"))))
              (assert (member-p recorder))
              (assert (member-p (find-package "BUILD-HEAP-SEALED")))
              (assert (member-p (sb-kernel:find-layout
                                 (find-symbol "POINT" "BUILD-HEAP-SEALED"))))
              (assert (member-p (sb-kernel:fun-code-header (sb-kernel:%fun-fun norm))))
              (assert (member-p *build-heap-sealed-value*))
              (assert (not (member-p stray)))
              ;; Global objects are never members, whatever refers to them.
              (assert (not (member-p (find-package "CL"))))
              (let ((record (find #+linkage-space 'build-heap-sealed-hook
                                                   #-linkage-space (sb-int:find-fdefn 'build-heap-sealed-hook)
                                  (records-of-kind recorder :set-function)
                                  :key #'fragment-record-target)))
                (assert record)
                (assert (equal (fragment-record-replay-values record) (list norm))))))
          ;; Its allocation is over.
          (assert-error (with-heap (heap) (list 1)))
          (sb-ext:gc :full t)
          (assert (equal *build-heap-sealed-value* '(:sealed))))))))

;;; --- Writing ---

;;; The fragment writer lays a sealed fragment out at planned addresses,
;;; packed by page type, and writes it; reading the file back checks that
;;; every relocated word points into the fragment at an object, that no
;;; other word does, and that the counts agree with what was sealed.
(with-test (:name (:build-heap :write :verifies))
  (load "../tools-for-build/corefile.lisp")
  (load "../tools-for-build/corefrag-writer.lisp")
  (with-scratch-file (source "lisp")
    (with-scratch-file (fasl "fasl")
      (with-scratch-file (file "sbfr")
        (with-open-file (s source :direction :output :if-exists :supersede)
          (write-string "(defpackage \"BUILD-HEAP-WRITTEN\" (:use \"CL\"))
(in-package \"BUILD-HEAP-WRITTEN\")
(defstruct point x y)
(defun norm (p) (+ (abs (point-x p)) (abs (point-y p))))
(defgeneric area (shape))
(defmethod area ((p point)) (* (point-x p) (point-y p)))
(defun make-adder (n) (lambda (x) (+ x n)))
(defvar *adder* (make-adder 3))
(defvar *table* (let ((h (make-hash-table :test 'equal))) (setf (gethash \"k\" h) (list 1 2 3)) h))
(defvar *big* (make-array 4000 :initial-element nil))
" s))
        (compile-file source :output-file fasl)
        (when (find-package "BUILD-HEAP-WRITTEN") (delete-package "BUILD-HEAP-WRITTEN"))
        (let* ((heap (make-heap :kind :build :check-stores :error))
               (recorder (make-fragment-recorder heap)))
          (with-open-file (stream fasl :element-type '(unsigned-byte 8))
            (with-fragment-recorder (recorder)
              (with-heap (heap) (load stream))))
          (assert-all-accounted recorder)
          (let* ((count (seal-fragment recorder))
                 (base (+ sb-vm:dynamic-space-start (* 3 (floor (sb-ext:dynamic-space-size) 4))))
                 (written (funcall (intern "WRITE-FRAGMENT" "SB-COREFRAG-WRITER") recorder file :base base))
                 (verified (funcall (intern "VERIFY-FRAGMENT-FILE" "SB-COREFRAG-WRITER") file)))
            (assert (= (getf written :members) count))
            (assert (= (getf verified :members) count))
            (assert (= (getf verified :pointers) (getf written :pointers)))
            ;; The 4000-element vector takes pages of its own: at least four runs.
            (assert (>= (getf written :runs) 4))
            (assert (plusp (getf verified :exports)))))))))

;;; --- Activating ---

;;; A fragment written by one process is read back by a fresh process
;;; started from the same core: its runs are mapped at their planned
;;; addresses and installed as pseudo-static pages, and its functions run
;;; there, with the struct's layout and the base core's linkage cells.
(with-test (:name (:build-heap :activate :in-a-fresh-process))
  (load "../tools-for-build/corefile.lisp")
  (load "../tools-for-build/corefrag-writer.lisp")
  (with-scratch-file (source "lisp")
    (with-scratch-file (fasl "fasl")
      (with-scratch-file (file "sbfr")
        (with-open-file (s source :direction :output :if-exists :supersede)
          (write-string "(defpackage \"BUILD-HEAP-ACTIVATED\" (:use \"CL\"))
(in-package \"BUILD-HEAP-ACTIVATED\")
(defstruct point x y)
(defun norm (p) (+ (abs (point-x p)) (abs (point-y p))))
(defvar *p* (make-point :x -3 :y 4))
" s))
        (compile-file source :output-file fasl)
        (when (find-package "BUILD-HEAP-ACTIVATED") (delete-package "BUILD-HEAP-ACTIVATED"))
        (let* ((heap (make-heap :kind :build :check-stores :error))
               (recorder (make-fragment-recorder heap)))
          (with-open-file (stream fasl :element-type '(unsigned-byte 8))
            (with-fragment-recorder (recorder)
              (with-heap (heap) (load stream))))
          (assert-all-accounted recorder)
          (let* ((count (seal-fragment recorder))
                 (base (+ sb-vm:dynamic-space-start (* 3 (floor (sb-ext:dynamic-space-size) 4))))
                 (written (funcall (intern "WRITE-FRAGMENT" "SB-COREFRAG-WRITER") recorder file :base base))
                 (exports (getf (funcall (intern "FRAGMENT-FILE-STATS" "SB-COREFRAG-WRITER") file) :exports))
                 (norm (car (last (find-if (lambda (e) (and (eq (first e) :function) (string= (second e) "NORM"))) exports))))
                 (p (car (last (find-if (lambda (e) (and (eq (first e) :symbol) (string= (second e) "*P*"))) exports)))))
            (assert (= (getf written :members) count))
            (assert (and norm p))
            ;; The child activates the file and calls NORM, through its simple-fun,
            ;; on the point *P* holds, read from the symbol's global value slot: a
            ;; full call to a function of the fragment would go through a linkage
            ;; cell only the builder assigned, which is the record replay's work.
            ;; It prints the run count, the member count it sees, and the result.
            (let* ((forms (format nil "(multiple-value-bind (runs bytes) (sb-fiber:activate-fragment-file ~S) (let ((n 0)) (sb-vm:map-allocated-objects (lambda (x type size) (declare (ignore type size)) (let ((a (sb-kernel:get-lisp-obj-address x))) (when (and (>= a ~D) (< a (+ ~D bytes))) (incf n)))) :dynamic) (format t \"~~&RESULT ~~D ~~D ~~D~~%\" runs n (funcall (sb-kernel:%make-lisp-obj ~D) (sb-ext:symbol-global-value (sb-kernel:%make-lisp-obj ~D))))))"
                                  (namestring file) base base norm p))
                   (output (with-output-to-string (s)
                             (run-program sb-ext:*runtime-pathname*
                                          (list "--core" (namestring sb-ext:*core-pathname*)
                                                "--dynamic-space-size" (format nil "~DMB" (floor (sb-ext:dynamic-space-size) (* 1024 1024)))
                                                "--noinform" "--non-interactive" "--no-sysinit" "--no-userinit"
                                                "--eval" "(require :sb-fiber)" "--eval" forms)
                                          :output s :error s :search nil)))
                   (start (search "RESULT " output)))
              (unless start (error "the child did not report: ~A" output))
              (destructuring-bind (runs seen result)
                  (handler-case (with-input-from-string (s output :start (+ start 7))
                                  (let ((v (list (read s) (read s) (read s))))
                                    (unless (every (function integerp) v) (error "not integers"))
                                    v))
                    (error () (error "the child reported ~A" output)))
                (assert (= runs (getf written :runs)))
                (assert (= seen count))
                (assert (= result 7))))))))))
