;;;; core-graphics-example.lisp - Core Graphics with struct-by-value returns
;;;;
;;;; Demonstrates using Apple's Core Graphics (Quartz 2D) from SBCL.
;;;; Many CG functions return small structs like CGPoint, CGSize, CGRect.
;;;;
;;;; Requires: macOS with ApplicationServices framework
;;;; Uses: struct-by-value returns (srbv branch feature)

(defpackage #:cg-example
  (:use #:cl)
  (:export #:run-demo))

(in-package #:cg-example)

;;; ============================================================================
;;; Core Graphics Struct Definitions
;;; ============================================================================

;;; CGFloat is double on 64-bit macOS
(define-alien-type cg-float double)

;;; CGPoint - 16 bytes, returned in registers (xmm0/xmm1 or d0/d1)
(define-alien-type cg-point
    (struct cg-point
      (x cg-float)
      (y cg-float)))

;;; CGSize - 16 bytes, returned in registers
(define-alien-type cg-size
    (struct cg-size
      (width cg-float)
      (height cg-float)))

;;; CGRect - 32 bytes, returned via hidden pointer (too large for registers)
(define-alien-type cg-rect
    (struct cg-rect
      (origin cg-point)
      (size cg-size)))

;;; CGAffineTransform - 48 bytes, returned via hidden pointer
(define-alien-type cg-affine-transform
    (struct cg-affine-transform
      (a cg-float)
      (b cg-float)
      (c cg-float)
      (d cg-float)
      (tx cg-float)
      (ty cg-float)))

;;; ============================================================================
;;; Load Core Graphics Framework
;;; ============================================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-case
      (load-shared-object "/System/Library/Frameworks/ApplicationServices.framework/ApplicationServices")
    (error (c)
      (warn "Could not load ApplicationServices framework: ~A~%This demo requires macOS." c))))

;;; ============================================================================
;;; Point and Size Constructors (inline functions returning small structs)
;;; ============================================================================

;;; CGPointMake and CGSizeMake are inline in the headers, so we define them
(defun make-cg-point (x y)
  "Create a CGPoint. Returns a freshly allocated struct."
  (let ((pt (make-alien cg-point)))
    (setf (slot pt 'x) (coerce x 'double-float)
          (slot pt 'y) (coerce y 'double-float))
    pt))

(defun make-cg-size (width height)
  "Create a CGSize. Returns a freshly allocated struct."
  (let ((sz (make-alien cg-size)))
    (setf (slot sz 'width) (coerce width 'double-float)
          (slot sz 'height) (coerce height 'double-float))
    sz))

(defun make-cg-rect (x y width height)
  "Create a CGRect. Returns a freshly allocated struct."
  (let ((rect (make-alien cg-rect)))
    (setf (slot (slot rect 'origin) 'x) (coerce x 'double-float)
          (slot (slot rect 'origin) 'y) (coerce y 'double-float)
          (slot (slot rect 'size) 'width) (coerce width 'double-float)
          (slot (slot rect 'size) 'height) (coerce height 'double-float))
    rect))

;;; ============================================================================
;;; Affine Transform Functions
;;; ============================================================================

;;; These are real library functions that return structs by value

(declaim (inline %cg-affine-transform-identity))
(define-alien-routine ("CGAffineTransformIdentity" %cg-affine-transform-identity)
    cg-affine-transform)

;; Note: CGAffineTransformIdentity is actually a global variable, not a function.
;; The actual transform functions are:

(define-alien-routine ("CGAffineTransformMake" cg-affine-transform-make)
    cg-affine-transform
  (a cg-float)
  (b cg-float)
  (c cg-float)
  (d cg-float)
  (tx cg-float)
  (ty cg-float))

(define-alien-routine ("CGAffineTransformMakeTranslation" cg-affine-transform-make-translation)
    cg-affine-transform
  (tx cg-float)
  (ty cg-float))

(define-alien-routine ("CGAffineTransformMakeScale" cg-affine-transform-make-scale)
    cg-affine-transform
  (sx cg-float)
  (sy cg-float))

(define-alien-routine ("CGAffineTransformMakeRotation" cg-affine-transform-make-rotation)
    cg-affine-transform
  (angle cg-float))

(define-alien-routine ("CGAffineTransformTranslate" cg-affine-transform-translate)
    cg-affine-transform
  (t cg-affine-transform)
  (tx cg-float)
  (ty cg-float))

(define-alien-routine ("CGAffineTransformScale" cg-affine-transform-scale)
    cg-affine-transform
  (t cg-affine-transform)
  (sx cg-float)
  (sy cg-float))

(define-alien-routine ("CGAffineTransformRotate" cg-affine-transform-rotate)
    cg-affine-transform
  (t cg-affine-transform)
  (angle cg-float))

(define-alien-routine ("CGAffineTransformConcat" cg-affine-transform-concat)
    cg-affine-transform
  (t1 cg-affine-transform)
  (t2 cg-affine-transform))

(define-alien-routine ("CGAffineTransformInvert" cg-affine-transform-invert)
    cg-affine-transform
  (t cg-affine-transform))

;;; ============================================================================
;;; Point Transformation
;;; ============================================================================

(define-alien-routine ("CGPointApplyAffineTransform" cg-point-apply-affine-transform)
    cg-point
  (point cg-point)
  (t cg-affine-transform))

;;; ============================================================================
;;; Rectangle Functions
;;; ============================================================================

(define-alien-routine ("CGRectStandardize" cg-rect-standardize)
    cg-rect
  (rect cg-rect))

(define-alien-routine ("CGRectIntegral" cg-rect-integral)
    cg-rect
  (rect cg-rect))

(define-alien-routine ("CGRectUnion" cg-rect-union)
    cg-rect
  (r1 cg-rect)
  (r2 cg-rect))

(define-alien-routine ("CGRectIntersection" cg-rect-intersection)
    cg-rect
  (r1 cg-rect)
  (r2 cg-rect))

(define-alien-routine ("CGRectOffset" cg-rect-offset)
    cg-rect
  (rect cg-rect)
  (dx cg-float)
  (dy cg-float))

(define-alien-routine ("CGRectInset" cg-rect-inset)
    cg-rect
  (rect cg-rect)
  (dx cg-float)
  (dy cg-float))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(defun print-point (pt &optional (stream *standard-output*))
  "Print a CGPoint."
  (format stream "(~,2F, ~,2F)" (slot pt 'x) (slot pt 'y)))

(defun print-size (sz &optional (stream *standard-output*))
  "Print a CGSize."
  (format stream "~,2F x ~,2F" (slot sz 'width) (slot sz 'height)))

(defun print-rect (rect &optional (stream *standard-output*))
  "Print a CGRect."
  (format stream "[origin: (~,2F, ~,2F), size: ~,2F x ~,2F]"
          (slot (slot rect 'origin) 'x)
          (slot (slot rect 'origin) 'y)
          (slot (slot rect 'size) 'width)
          (slot (slot rect 'size) 'height)))

(defun print-transform (xform &optional (stream *standard-output*))
  "Print a CGAffineTransform as a 3x3 matrix (bottom row implicit)."
  (format stream "| ~7,3F ~7,3F  0 |~%| ~7,3F ~7,3F  0 |~%| ~7,3F ~7,3F  1 |"
          (slot xform 'a) (slot xform 'c)
          (slot xform 'b) (slot xform 'd)
          (slot xform 'tx) (slot xform 'ty)))

;;; ============================================================================
;;; Demo
;;; ============================================================================

(defun demo-transforms ()
  "Demonstrate affine transform composition and point transformation."
  (format t "~%=== Core Graphics Affine Transform Demo ===~%~%")

  ;; Create identity transform
  (let ((identity (cg-affine-transform-make 1.0d0 0.0d0 0.0d0 1.0d0 0.0d0 0.0d0)))
    (format t "Identity transform:~%")
    (print-transform identity)
    (format t "~%~%"))

  ;; Create translation
  (let ((translate (cg-affine-transform-make-translation 100.0d0 50.0d0)))
    (format t "Translation (100, 50):~%")
    (print-transform translate)
    (format t "~%~%"))

  ;; Create rotation (45 degrees)
  (let* ((angle (/ pi 4.0d0))
         (rotate (cg-affine-transform-make-rotation angle)))
    (format t "Rotation (45 degrees):~%")
    (print-transform rotate)
    (format t "~%~%"))

  ;; Create scale
  (let ((scale (cg-affine-transform-make-scale 2.0d0 0.5d0)))
    (format t "Scale (2x, 0.5x):~%")
    (print-transform scale)
    (format t "~%~%"))

  ;; Compose transforms: translate, then rotate, then scale
  (let* ((t1 (cg-affine-transform-make-translation 10.0d0 20.0d0))
         (t2 (cg-affine-transform-rotate t1 (/ pi 6.0d0)))  ; 30 degrees
         (t3 (cg-affine-transform-scale t2 1.5d0 1.5d0)))
    (format t "Composed (translate -> rotate -> scale):~%")
    (print-transform t3)
    (format t "~%~%")

    ;; Apply to a point
    (let ((pt (make-cg-point 1.0 0.0)))
      (format t "Original point: ")
      (print-point pt)
      (format t "~%")

      (let ((transformed (cg-point-apply-affine-transform pt t3)))
        (format t "Transformed point: ")
        (print-point transformed)
        (format t "~%~%")

        ;; Clean up
        (free-alien pt)
        (free-alien transformed)))

    ;; Invert the transform
    (let ((inv (cg-affine-transform-invert t3)))
      (format t "Inverted transform:~%")
      (print-transform inv)
      (format t "~%"))))

(defun demo-rectangles ()
  "Demonstrate rectangle operations."
  (format t "~%=== Core Graphics Rectangle Demo ===~%~%")

  (let ((r1 (make-cg-rect 10 20 100 50))
        (r2 (make-cg-rect 50 40 80 60)))

    (format t "Rectangle 1: ")
    (print-rect r1)
    (format t "~%")

    (format t "Rectangle 2: ")
    (print-rect r2)
    (format t "~%~%")

    ;; Union
    (let ((union (cg-rect-union r1 r2)))
      (format t "Union: ")
      (print-rect union)
      (format t "~%")
      (free-alien union))

    ;; Intersection
    (let ((intersection (cg-rect-intersection r1 r2)))
      (format t "Intersection: ")
      (print-rect intersection)
      (format t "~%~%"))

    ;; Offset
    (let ((offset (cg-rect-offset r1 5.0d0 -5.0d0)))
      (format t "R1 offset by (5, -5): ")
      (print-rect offset)
      (format t "~%")
      (free-alien offset))

    ;; Inset
    (let ((inset (cg-rect-inset r1 10.0d0 5.0d0)))
      (format t "R1 inset by (10, 5): ")
      (print-rect inset)
      (format t "~%")
      (free-alien inset))

    ;; Clean up
    (free-alien r1)
    (free-alien r2)))

(defun run-demo ()
  "Run the full Core Graphics demo."
  (format t "~%Core Graphics (Quartz 2D) Struct-by-Value Demo~%")
  (format t "================================================~%")
  (format t "~%This demo shows how SBCL's struct-by-value FFI support~%")
  (format t "enables natural use of Core Graphics functions that return~%")
  (format t "structures like CGPoint (16 bytes), CGRect (32 bytes), and~%")
  (format t "CGAffineTransform (48 bytes).~%")

  (demo-transforms)
  (demo-rectangles)

  (format t "~%Demo complete.~%"))

;;; ============================================================================
;;; Notes on ABI behavior
;;; ============================================================================
#|
Structure sizes and return conventions on macOS:

CGPoint (16 bytes):
  - x86-64: Two SSE eightbytes -> xmm0, xmm1
  - ARM64: Two doubles -> d0, d1 (or as HFA in s0-s3)

CGSize (16 bytes):
  - Same as CGPoint

CGRect (32 bytes):
  - x86-64: Hidden pointer in RDI, returned in RAX
  - ARM64: Hidden pointer in x8

CGAffineTransform (48 bytes):
  - x86-64: Hidden pointer in RDI, returned in RAX
  - ARM64: Hidden pointer in x8

These demonstrate both small-struct register returns and
large-struct hidden pointer returns in the same API.
|#
