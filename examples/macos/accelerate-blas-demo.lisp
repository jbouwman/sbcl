;;;; accelerate-blas-demo.lisp
;;;; Complete BLAS example using Apple's Accelerate framework
;;;;
;;;; To run:
;;;;   1. Load this file in SBCL on macOS
;;;;   2. (blas-demo)

(in-package :cl-user)

;;; ============================================================
;;; Load Accelerate Framework
;;; ============================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load-shared-object "/System/Library/Frameworks/Accelerate.framework/Accelerate"))

;;; ============================================================
;;; CBLAS Constants
;;; ============================================================

(defconstant +cblas-row-major+ 101)
(defconstant +cblas-col-major+ 102)
(defconstant +cblas-no-trans+ 111)
(defconstant +cblas-trans+ 112)
(defconstant +cblas-conj-trans+ 113)
(defconstant +cblas-upper+ 121)
(defconstant +cblas-lower+ 122)
(defconstant +cblas-non-unit+ 131)
(defconstant +cblas-unit+ 132)
(defconstant +cblas-left+ 141)
(defconstant +cblas-right+ 142)

;;; ============================================================
;;; BLAS Level 1: Vector-Vector Operations
;;; ============================================================

;; Dot product: result = x · y
(define-alien-routine ("cblas_ddot" %ddot) double-float
  (n (signed 32))
  (x (* double-float))
  (incx (signed 32))
  (y (* double-float))
  (incy (signed 32)))

;; Euclidean norm: result = ||x||₂
(define-alien-routine ("cblas_dnrm2" %dnrm2) double-float
  (n (signed 32))
  (x (* double-float))
  (incx (signed 32)))

;; Sum of absolute values: result = ||x||₁
(define-alien-routine ("cblas_dasum" %dasum) double-float
  (n (signed 32))
  (x (* double-float))
  (incx (signed 32)))

;; Index of max absolute value
(define-alien-routine ("cblas_idamax" %idamax) (signed 32)
  (n (signed 32))
  (x (* double-float))
  (incx (signed 32)))

;; Scale: x = alpha * x
(define-alien-routine ("cblas_dscal" %dscal) void
  (n (signed 32))
  (alpha double-float)
  (x (* double-float))
  (incx (signed 32)))

;; AXPY: y = alpha * x + y
(define-alien-routine ("cblas_daxpy" %daxpy) void
  (n (signed 32))
  (alpha double-float)
  (x (* double-float))
  (incx (signed 32))
  (y (* double-float))
  (incy (signed 32)))

;; Copy: y = x
(define-alien-routine ("cblas_dcopy" %dcopy) void
  (n (signed 32))
  (x (* double-float))
  (incx (signed 32))
  (y (* double-float))
  (incy (signed 32)))

;; Swap: x <-> y
(define-alien-routine ("cblas_dswap" %dswap) void
  (n (signed 32))
  (x (* double-float))
  (incx (signed 32))
  (y (* double-float))
  (incy (signed 32)))

;;; ============================================================
;;; BLAS Level 2: Matrix-Vector Operations
;;; ============================================================

;; General matrix-vector: y = alpha * A * x + beta * y
(define-alien-routine ("cblas_dgemv" %dgemv) void
  (order (signed 32))
  (trans (signed 32))
  (m (signed 32))              ; rows of A
  (n (signed 32))              ; cols of A
  (alpha double-float)
  (a (* double-float))
  (lda (signed 32))
  (x (* double-float))
  (incx (signed 32))
  (beta double-float)
  (y (* double-float))
  (incy (signed 32)))

;;; ============================================================
;;; BLAS Level 3: Matrix-Matrix Operations
;;; ============================================================

;; General matrix-matrix: C = alpha * A * B + beta * C
(define-alien-routine ("cblas_dgemm" %dgemm) void
  (order (signed 32))
  (trans-a (signed 32))
  (trans-b (signed 32))
  (m (signed 32))              ; rows of C
  (n (signed 32))              ; cols of C
  (k (signed 32))              ; cols of A / rows of B
  (alpha double-float)
  (a (* double-float))
  (lda (signed 32))
  (b (* double-float))
  (ldb (signed 32))
  (beta double-float)
  (c (* double-float))
  (ldc (signed 32)))

;;; ============================================================
;;; Lisp Vector Type
;;; ============================================================

(deftype dvec () '(simple-array double-float (*)))

(defun make-dvec (n &optional (initial-element 0.0d0))
  "Create a double-float vector of length N."
  (make-array n :element-type 'double-float
                :initial-element initial-element))

(defun dvec (&rest elements)
  "Create a double-float vector from elements."
  (make-array (length elements)
              :element-type 'double-float
              :initial-contents (mapcar (lambda (x) (coerce x 'double-float))
                                        elements)))

(defun dvec-copy (v)
  "Copy a vector."
  (let ((result (make-dvec (length v))))
    (replace result v)
    result))

(defmacro with-dvec-ptr ((ptr vec) &body body)
  "Execute BODY with PTR bound to the SAP of VEC's data."
  `(sb-sys:with-pinned-objects (,vec)
     (let ((,ptr (sb-sys:vector-sap ,vec)))
       ,@body)))

(defmacro with-dvec-ptrs (bindings &body body)
  "Execute BODY with multiple vector pointers bound."
  (if (null bindings)
      `(progn ,@body)
      `(with-dvec-ptr ,(first bindings)
         (with-dvec-ptrs ,(rest bindings)
           ,@body))))

;;; ============================================================
;;; Lisp Matrix Type (Row-Major)
;;; ============================================================

(defstruct (dmat (:constructor %make-dmat))
  (rows 0 :type fixnum)
  (cols 0 :type fixnum)
  (data nil :type dvec))

(defun make-dmat (rows cols &optional (initial-element 0.0d0))
  "Create a ROWSxCOLS matrix."
  (%make-dmat :rows rows
              :cols cols
              :data (make-dvec (* rows cols) initial-element)))

(defun dmat (rows cols &rest elements)
  "Create a matrix from elements (row-major order)."
  (let ((m (make-dmat rows cols)))
    (loop for e in elements
          for i from 0
          do (setf (aref (dmat-data m) i) (coerce e 'double-float)))
    m))

(defun dmat-ref (m row col)
  "Get element at (ROW, COL)."
  (aref (dmat-data m) (+ (* row (dmat-cols m)) col)))

(defun (setf dmat-ref) (value m row col)
  "Set element at (ROW, COL)."
  (setf (aref (dmat-data m) (+ (* row (dmat-cols m)) col))
        (coerce value 'double-float)))

(defun dmat-copy (m)
  "Copy a matrix."
  (%make-dmat :rows (dmat-rows m)
              :cols (dmat-cols m)
              :data (dvec-copy (dmat-data m))))

(defmacro with-dmat-ptr ((ptr mat) &body body)
  "Execute BODY with PTR bound to the SAP of MAT's data."
  `(with-dvec-ptr (,ptr (dmat-data ,mat))
     ,@body))

;;; ============================================================
;;; High-Level BLAS Wrappers
;;; ============================================================

;;; Level 1

(defun blas-dot (x y)
  "Dot product of vectors X and Y."
  (assert (= (length x) (length y)))
  (with-dvec-ptrs ((px x) (py y))
    (%ddot (length x) px 1 py 1)))

(defun blas-norm (x)
  "Euclidean norm (L2) of vector X."
  (with-dvec-ptr (px x)
    (%dnrm2 (length x) px 1)))

(defun blas-asum (x)
  "Sum of absolute values (L1 norm) of vector X."
  (with-dvec-ptr (px x)
    (%dasum (length x) px 1)))

(defun blas-iamax (x)
  "Index of element with maximum absolute value."
  (with-dvec-ptr (px x)
    (%idamax (length x) px 1)))

(defun blas-scale! (alpha x)
  "Scale vector X by ALPHA in place. Returns X."
  (with-dvec-ptr (px x)
    (%dscal (length x) (coerce alpha 'double-float) px 1))
  x)

(defun blas-axpy! (alpha x y)
  "Y = ALPHA * X + Y. Modifies Y in place. Returns Y."
  (assert (= (length x) (length y)))
  (with-dvec-ptrs ((px x) (py y))
    (%daxpy (length x) (coerce alpha 'double-float) px 1 py 1))
  y)

(defun blas-copy! (x y)
  "Copy X into Y. Returns Y."
  (assert (= (length x) (length y)))
  (with-dvec-ptrs ((px x) (py y))
    (%dcopy (length x) px 1 py 1))
  y)

(defun blas-swap! (x y)
  "Swap contents of X and Y. Returns (values X Y)."
  (assert (= (length x) (length y)))
  (with-dvec-ptrs ((px x) (py y))
    (%dswap (length x) px 1 py 1))
  (values x y))

;;; Level 2

(defun blas-gemv! (alpha a x beta y)
  "Y = ALPHA * A * X + BETA * Y. Modifies Y. Returns Y."
  (assert (= (dmat-cols a) (length x)))
  (assert (= (dmat-rows a) (length y)))
  (with-dvec-ptrs ((px x) (py y))
    (with-dmat-ptr (pa a)
      (%dgemv +cblas-row-major+ +cblas-no-trans+
              (dmat-rows a) (dmat-cols a)
              (coerce alpha 'double-float)
              pa (dmat-cols a)
              px 1
              (coerce beta 'double-float)
              py 1)))
  y)

(defun blas-mat-vec (a x)
  "Matrix-vector product A * X. Returns new vector."
  (let ((y (make-dvec (dmat-rows a) 0.0d0)))
    (blas-gemv! 1.0d0 a x 0.0d0 y)
    y))

;;; Level 3

(defun blas-gemm! (alpha a b beta c)
  "C = ALPHA * A * B + BETA * C. Modifies C. Returns C."
  (assert (= (dmat-cols a) (dmat-rows b)))
  (assert (= (dmat-rows a) (dmat-rows c)))
  (assert (= (dmat-cols b) (dmat-cols c)))
  (with-dmat-ptr (pa a)
    (with-dmat-ptr (pb b)
      (with-dmat-ptr (pc c)
        (%dgemm +cblas-row-major+ +cblas-no-trans+ +cblas-no-trans+
                (dmat-rows a) (dmat-cols b) (dmat-cols a)
                (coerce alpha 'double-float)
                pa (dmat-cols a)
                pb (dmat-cols b)
                (coerce beta 'double-float)
                pc (dmat-cols c)))))
  c)

(defun blas-mat-mul (a b)
  "Matrix product A * B. Returns new matrix."
  (let ((c (make-dmat (dmat-rows a) (dmat-cols b) 0.0d0)))
    (blas-gemm! 1.0d0 a b 0.0d0 c)
    c))

(defun blas-mat-mul-transposed (a b)
  "Matrix product A * B'. Returns new matrix."
  (assert (= (dmat-cols a) (dmat-cols b)))
  (let ((c (make-dmat (dmat-rows a) (dmat-rows b) 0.0d0)))
    (with-dmat-ptr (pa a)
      (with-dmat-ptr (pb b)
        (with-dmat-ptr (pc c)
          (%dgemm +cblas-row-major+ +cblas-no-trans+ +cblas-trans+
                  (dmat-rows a) (dmat-rows b) (dmat-cols a)
                  1.0d0
                  pa (dmat-cols a)
                  pb (dmat-cols b)
                  0.0d0
                  pc (dmat-rows b)))))
    c))

;;; ============================================================
;;; Convenience Functions
;;; ============================================================

(defun v+ (x y)
  "Vector addition. Returns new vector."
  (let ((result (dvec-copy x)))
    (blas-axpy! 1.0d0 y result)
    result))

(defun v- (x y)
  "Vector subtraction. Returns new vector."
  (let ((result (dvec-copy x)))
    (blas-axpy! -1.0d0 y result)
    result))

(defun v* (alpha x)
  "Scalar-vector multiplication. Returns new vector."
  (let ((result (dvec-copy x)))
    (blas-scale! alpha result)
    result))

(defun normalize (x)
  "Return normalized (unit) vector."
  (let ((norm (blas-norm x)))
    (if (zerop norm)
        (dvec-copy x)
        (v* (/ 1.0d0 norm) x))))

(defun cross (a b)
  "Cross product of 3D vectors."
  (assert (= (length a) (length b) 3))
  (dvec (- (* (aref a 1) (aref b 2)) (* (aref a 2) (aref b 1)))
        (- (* (aref a 2) (aref b 0)) (* (aref a 0) (aref b 2)))
        (- (* (aref a 0) (aref b 1)) (* (aref a 1) (aref b 0)))))

;;; ============================================================
;;; Printing
;;; ============================================================

(defun print-dvec (v &optional (stream t) (precision 4))
  "Print a vector."
  (format stream "[")
  (loop for i below (length v)
        do (format stream "~,vF" precision (aref v i))
        when (< i (1- (length v))) do (format stream " "))
  (format stream "]")
  (terpri stream))

(defun print-dmat (m &optional (stream t) (precision 4))
  "Print a matrix."
  (loop for row below (dmat-rows m)
        do (format stream (if (zerop row) "[" " "))
           (loop for col below (dmat-cols m)
                 do (format stream "~10,vF" precision (dmat-ref m row col)))
           (format stream (if (= row (1- (dmat-rows m))) " ]~%" "~%"))))

;;; ============================================================
;;; Demo Functions
;;; ============================================================

(defun demo-level-1 ()
  "Demonstrate BLAS Level 1 operations."
  (format t "~%===== BLAS Level 1: Vector Operations =====~%~%")

  (let ((x (dvec 1 2 3 4 5))
        (y (dvec 5 4 3 2 1)))

    (format t "Vector x: ") (print-dvec x)
    (format t "Vector y: ") (print-dvec y)
    (format t "~%")

    ;; Dot product
    (format t "x . y (dot product): ~,4F~%" (blas-dot x y))

    ;; Norms
    (format t "||x||_2 (Euclidean norm): ~,4F~%" (blas-norm x))
    (format t "||x||_1 (sum of |x|): ~,4F~%" (blas-asum x))

    ;; Max element
    (let ((imax (blas-iamax x)))
      (format t "Index of max |x|: ~A (value: ~,4F)~%" imax (aref x imax)))

    ;; AXPY
    (let ((result (v+ x (v* 2.0d0 y))))
      (format t "~%x + 2*y: ") (print-dvec result))

    ;; Normalize
    (format t "normalize(x): ") (print-dvec (normalize x))
    (format t "||normalize(x)||: ~,4F~%" (blas-norm (normalize x)))))

(defun demo-level-2 ()
  "Demonstrate BLAS Level 2 operations."
  (format t "~%===== BLAS Level 2: Matrix-Vector Operations =====~%~%")

  (let ((a (dmat 3 3
                 1 2 3
                 4 5 6
                 7 8 9))
        (x (dvec 1 1 1)))

    (format t "Matrix A:~%")
    (print-dmat a)
    (format t "~%Vector x: ") (print-dvec x)

    ;; Matrix-vector multiply
    (let ((result (blas-mat-vec a x)))
      (format t "~%A * x: ") (print-dvec result))

    ;; With scaling
    (let* ((y (dvec 10 20 30))
           (result (make-dvec 3)))
      (replace result y)
      (blas-gemv! 2.0d0 a x 0.5d0 result)
      (format t "~%y: ") (print-dvec y)
      (format t "2*A*x + 0.5*y: ") (print-dvec result))))

(defun demo-level-3 ()
  "Demonstrate BLAS Level 3 operations."
  (format t "~%===== BLAS Level 3: Matrix-Matrix Operations =====~%~%")

  (let ((a (dmat 2 3
                 1 2 3
                 4 5 6))
        (b (dmat 3 2
                 7 8
                 9 10
                 11 12)))

    (format t "Matrix A (2x3):~%")
    (print-dmat a)
    (format t "~%Matrix B (3x2):~%")
    (print-dmat b)

    ;; Matrix multiply
    (let ((c (blas-mat-mul a b)))
      (format t "~%A * B (2x2):~%")
      (print-dmat c)))

  ;; Square matrices
  (format t "~%--- Square Matrix Operations ---~%~%")

  (let ((m (dmat 3 3
                 1 2 0
                 0 1 2
                 2 0 1)))

    (format t "Matrix M:~%")
    (print-dmat m)

    ;; M * M
    (let ((m2 (blas-mat-mul m m)))
      (format t "~%M^2:~%")
      (print-dmat m2))

    ;; M * M^T
    (let ((mmt (blas-mat-mul-transposed m m)))
      (format t "~%M * M^T (symmetric):~%")
      (print-dmat mmt))))

(defun demo-3d-graphics ()
  "Demonstrate 3D graphics transformations using BLAS."
  (format t "~%===== 3D Graphics Transformations =====~%~%")

  ;; Define transformation matrices (4x4 homogeneous)
  (let* ((translate (dmat 4 4
                          1 0 0 10
                          0 1 0 20
                          0 0 1 30
                          0 0 0 1))

         (scale (dmat 4 4
                      2 0 0 0
                      0 2 0 0
                      0 0 2 0
                      0 0 0 1))

         ;; Rotation around Z axis by 90 degrees
         (cos90 0.0d0)
         (sin90 1.0d0)
         (rotate-z (dmat 4 4
                         cos90 (- sin90) 0 0
                         sin90 cos90    0 0
                         0     0        1 0
                         0     0        0 1))

         ;; A point in homogeneous coordinates
         (point (dvec 1 0 0 1)))

    (format t "Original point: ") (print-dvec (subseq point 0 3))

    ;; Apply translation
    (let ((translated (blas-mat-vec translate point)))
      (format t "After translation (+10, +20, +30): ")
      (print-dvec (subseq translated 0 3)))

    ;; Apply rotation
    (let ((rotated (blas-mat-vec rotate-z point)))
      (format t "After 90 deg Z rotation: ")
      (print-dvec (subseq rotated 0 3)))

    ;; Chain transformations: scale, then rotate, then translate
    (let* ((sr (blas-mat-mul rotate-z scale))
           (srt (blas-mat-mul translate sr))
           (result (blas-mat-vec srt point)))
      (format t "~%Combined: Scale(2) -> Rotate(90) -> Translate(10,20,30)~%")
      (format t "Result: ")
      (print-dvec (subseq result 0 3)))))

(defun demo-performance ()
  "Demonstrate BLAS performance on larger matrices."
  (format t "~%===== Performance Demo =====~%~%")

  (let* ((size 500)
         (a (make-dmat size size))
         (b (make-dmat size size))
         (c (make-dmat size size)))

    ;; Initialize with random values
    (dotimes (i (* size size))
      (setf (aref (dmat-data a) i) (random 1.0d0))
      (setf (aref (dmat-data b) i) (random 1.0d0)))

    (format t "Matrix size: ~Ax~A (~:D elements each)~%" size size (* size size))
    (format t "Total FLOPs for multiply: ~:D~%" (* 2 size size size))

    ;; Time the multiplication
    (let ((start (get-internal-real-time)))
      (blas-gemm! 1.0d0 a b 0.0d0 c)
      (let* ((end (get-internal-real-time))
             (elapsed (/ (- end start) internal-time-units-per-second))
             (gflops (/ (* 2 size size size) elapsed 1e9)))
        (format t "Time: ~,3F seconds~%" elapsed)
        (format t "Performance: ~,2F GFLOPS~%" gflops)))

    ;; Verify a value
    (format t "~%Sample result C[0,0]: ~,6F~%" (dmat-ref c 0 0))))

(defun blas-demo ()
  "Run all BLAS demonstrations."
  (format t "~%")
  (format t "========================================================~%")
  (format t "        SBCL + Accelerate BLAS Demonstration~%")
  (format t "        Using Apple's Optimized Linear Algebra~%")
  (format t "========================================================~%")

  (demo-level-1)
  (demo-level-2)
  (demo-level-3)
  (demo-3d-graphics)
  (demo-performance)

  (format t "~%========================================================~%")
  (format t "                     Demo Complete~%")
  (format t "========================================================~%~%"))

;;; ============================================================
;;; Entry Point
;;; ============================================================

;; Uncomment to run automatically on load:
;; (blas-demo)

;; Or run manually:
;; CL-USER> (blas-demo)
