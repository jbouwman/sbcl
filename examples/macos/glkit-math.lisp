;;;; glkit-math.lisp
;;;; GLKit 3D math bindings for macOS
;;;;
;;;; GLKit provides real library symbols for common 3D math operations.
;;;; Note: GLKit is deprecated but still available on macOS.
;;;;
;;;; To run:
;;;;   1. Load this file in SBCL on macOS
;;;;   2. (glkit-demo)

(in-package :cl-user)

;;; ============================================================
;;; Load GLKit Framework
;;; ============================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load-shared-object "/System/Library/Frameworks/GLKit.framework/GLKit"))

;;; ============================================================
;;; GLKit Types
;;; ============================================================

;; GLKVector3 - 12 bytes, returned in registers
(define-alien-type glk-vector3
  (struct glk-vector3
    (x single-float)
    (y single-float)
    (z single-float)))

;; GLKVector4 - 16 bytes, returned in registers
(define-alien-type glk-vector4
  (struct glk-vector4
    (x single-float)
    (y single-float)
    (z single-float)
    (w single-float)))

;; GLKQuaternion - 16 bytes, returned in registers
(define-alien-type glk-quaternion
  (struct glk-quaternion
    (x single-float)
    (y single-float)
    (z single-float)
    (w single-float)))

;; GLKMatrix3 - 36 bytes, uses hidden pointer return
(define-alien-type glk-matrix3
  (struct glk-matrix3
    (m00 single-float) (m01 single-float) (m02 single-float)
    (m10 single-float) (m11 single-float) (m12 single-float)
    (m20 single-float) (m21 single-float) (m22 single-float)))

;; GLKMatrix4 - 64 bytes, uses hidden pointer return
(define-alien-type glk-matrix4
  (struct glk-matrix4
    (m00 single-float) (m01 single-float) (m02 single-float) (m03 single-float)
    (m10 single-float) (m11 single-float) (m12 single-float) (m13 single-float)
    (m20 single-float) (m21 single-float) (m22 single-float) (m23 single-float)
    (m30 single-float) (m31 single-float) (m32 single-float) (m33 single-float)))

;;; ============================================================
;;; Vector3 Operations
;;; ============================================================

(define-alien-routine ("GLKVector3Make" glk-vector3-make) glk-vector3
  (x single-float) (y single-float) (z single-float))

(define-alien-routine ("GLKVector3Normalize" glk-vector3-normalize) glk-vector3
  (v glk-vector3))

(define-alien-routine ("GLKVector3CrossProduct" glk-vector3-cross) glk-vector3
  (a glk-vector3) (b glk-vector3))

(define-alien-routine ("GLKVector3DotProduct" glk-vector3-dot) single-float
  (a glk-vector3) (b glk-vector3))

(define-alien-routine ("GLKVector3Length" glk-vector3-length) single-float
  (v glk-vector3))

(define-alien-routine ("GLKVector3Add" glk-vector3-add) glk-vector3
  (a glk-vector3) (b glk-vector3))

(define-alien-routine ("GLKVector3Subtract" glk-vector3-subtract) glk-vector3
  (a glk-vector3) (b glk-vector3))

(define-alien-routine ("GLKVector3MultiplyScalar" glk-vector3-scale) glk-vector3
  (v glk-vector3) (s single-float))

(define-alien-routine ("GLKVector3Negate" glk-vector3-negate) glk-vector3
  (v glk-vector3))

(define-alien-routine ("GLKVector3Lerp" glk-vector3-lerp) glk-vector3
  (a glk-vector3) (b glk-vector3) (t single-float))

(define-alien-routine ("GLKVector3Project" glk-vector3-project) glk-vector3
  (a glk-vector3) (b glk-vector3))

;;; ============================================================
;;; Vector4 Operations
;;; ============================================================

(define-alien-routine ("GLKVector4Make" glk-vector4-make) glk-vector4
  (x single-float) (y single-float) (z single-float) (w single-float))

(define-alien-routine ("GLKVector4Normalize" glk-vector4-normalize) glk-vector4
  (v glk-vector4))

(define-alien-routine ("GLKVector4DotProduct" glk-vector4-dot) single-float
  (a glk-vector4) (b glk-vector4))

(define-alien-routine ("GLKVector4Length" glk-vector4-length) single-float
  (v glk-vector4))

(define-alien-routine ("GLKVector4Add" glk-vector4-add) glk-vector4
  (a glk-vector4) (b glk-vector4))

(define-alien-routine ("GLKVector4Subtract" glk-vector4-subtract) glk-vector4
  (a glk-vector4) (b glk-vector4))

;;; ============================================================
;;; Quaternion Operations
;;; ============================================================

(define-alien-routine ("GLKQuaternionMake" glk-quaternion-make) glk-quaternion
  (x single-float) (y single-float) (z single-float) (w single-float))

(define-alien-routine ("GLKQuaternionMakeWithAngleAndAxis" glk-quaternion-angle-axis)
  glk-quaternion
  (radians single-float)
  (x single-float) (y single-float) (z single-float))

(define-alien-routine ("GLKQuaternionNormalize" glk-quaternion-normalize) glk-quaternion
  (q glk-quaternion))

(define-alien-routine ("GLKQuaternionMultiply" glk-quaternion-multiply) glk-quaternion
  (a glk-quaternion) (b glk-quaternion))

(define-alien-routine ("GLKQuaternionSlerp" glk-quaternion-slerp) glk-quaternion
  (a glk-quaternion) (b glk-quaternion) (t single-float))

(define-alien-routine ("GLKQuaternionRotateVector3" glk-quaternion-rotate-vector3)
  glk-vector3
  (q glk-quaternion) (v glk-vector3))

;;; ============================================================
;;; Matrix4 Operations - 64 bytes, uses hidden pointer return
;;; ============================================================

(define-alien-routine ("GLKMatrix4Identity" glk-matrix4-identity) glk-matrix4)

(define-alien-routine ("GLKMatrix4MakeTranslation" glk-matrix4-translation) glk-matrix4
  (tx single-float) (ty single-float) (tz single-float))

(define-alien-routine ("GLKMatrix4MakeScale" glk-matrix4-scale) glk-matrix4
  (sx single-float) (sy single-float) (sz single-float))

(define-alien-routine ("GLKMatrix4MakeRotation" glk-matrix4-rotation) glk-matrix4
  (radians single-float)
  (x single-float) (y single-float) (z single-float))

(define-alien-routine ("GLKMatrix4MakeXRotation" glk-matrix4-rotation-x) glk-matrix4
  (radians single-float))

(define-alien-routine ("GLKMatrix4MakeYRotation" glk-matrix4-rotation-y) glk-matrix4
  (radians single-float))

(define-alien-routine ("GLKMatrix4MakeZRotation" glk-matrix4-rotation-z) glk-matrix4
  (radians single-float))

(define-alien-routine ("GLKMatrix4MakeLookAt" glk-matrix4-look-at) glk-matrix4
  (eye-x single-float) (eye-y single-float) (eye-z single-float)
  (center-x single-float) (center-y single-float) (center-z single-float)
  (up-x single-float) (up-y single-float) (up-z single-float))

(define-alien-routine ("GLKMatrix4MakePerspective" glk-matrix4-perspective) glk-matrix4
  (fov-radians single-float)
  (aspect single-float)
  (near single-float)
  (far single-float))

(define-alien-routine ("GLKMatrix4MakeOrtho" glk-matrix4-ortho) glk-matrix4
  (left single-float) (right single-float)
  (bottom single-float) (top single-float)
  (near single-float) (far single-float))

(define-alien-routine ("GLKMatrix4Multiply" glk-matrix4-multiply) glk-matrix4
  (a glk-matrix4) (b glk-matrix4))

(define-alien-routine ("GLKMatrix4Invert" glk-matrix4-invert) glk-matrix4
  (m glk-matrix4)
  (is-invertible (* (signed 32))))

(define-alien-routine ("GLKMatrix4Transpose" glk-matrix4-transpose) glk-matrix4
  (m glk-matrix4))

(define-alien-routine ("GLKMatrix4MultiplyVector3" glk-matrix4-mul-vector3) glk-vector3
  (m glk-matrix4) (v glk-vector3))

(define-alien-routine ("GLKMatrix4MultiplyVector4" glk-matrix4-mul-vector4) glk-vector4
  (m glk-matrix4) (v glk-vector4))

(define-alien-routine ("GLKMatrix4MakeWithQuaternion" glk-matrix4-from-quaternion)
  glk-matrix4
  (q glk-quaternion))

;;; ============================================================
;;; Lisp Wrapper Types
;;; ============================================================

(defstruct (vec3 (:constructor vec3 (x y z)))
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (z 0.0 :type single-float))

(defstruct (vec4 (:constructor vec4 (x y z w)))
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (z 0.0 :type single-float)
  (w 0.0 :type single-float))

(defun vec3->glk (v)
  (glk-vector3-make (vec3-x v) (vec3-y v) (vec3-z v)))

(defun glk->vec3 (g)
  (vec3 (slot g 'x) (slot g 'y) (slot g 'z)))

(defun vec4->glk (v)
  (glk-vector4-make (vec4-x v) (vec4-y v) (vec4-z v) (vec4-w v)))

(defun glk->vec4 (g)
  (vec4 (slot g 'x) (slot g 'y) (slot g 'z) (slot g 'w)))

;;; ============================================================
;;; Printing
;;; ============================================================

(defun print-vec3 (v &optional (stream t))
  (format stream "(~,3F, ~,3F, ~,3F)"
          (slot v 'x) (slot v 'y) (slot v 'z)))

(defun print-vec4 (v &optional (stream t))
  (format stream "(~,3F, ~,3F, ~,3F, ~,3F)"
          (slot v 'x) (slot v 'y) (slot v 'z) (slot v 'w)))

(defun print-matrix4 (m &optional (stream t))
  (format stream "~%")
  (format stream "  [~7,3F ~7,3F ~7,3F ~7,3F]~%"
          (slot m 'm00) (slot m 'm01) (slot m 'm02) (slot m 'm03))
  (format stream "  [~7,3F ~7,3F ~7,3F ~7,3F]~%"
          (slot m 'm10) (slot m 'm11) (slot m 'm12) (slot m 'm13))
  (format stream "  [~7,3F ~7,3F ~7,3F ~7,3F]~%"
          (slot m 'm20) (slot m 'm21) (slot m 'm22) (slot m 'm23))
  (format stream "  [~7,3F ~7,3F ~7,3F ~7,3F]~%"
          (slot m 'm30) (slot m 'm31) (slot m 'm32) (slot m 'm33)))

;;; ============================================================
;;; Demo
;;; ============================================================

(defun glkit-demo ()
  "Demonstrate GLKit vector and matrix operations."
  (format t "~%")
  (format t "========================================================~%")
  (format t "              GLKit 3D Math Demo~%")
  (format t "========================================================~%")

  ;; Vector operations
  (format t "~%--- Vector Operations ---~%~%")

  (let* ((a (glk-vector3-make 1.0 0.0 0.0))
         (b (glk-vector3-make 0.0 1.0 0.0))
         (cross (glk-vector3-cross a b))
         (dot (glk-vector3-dot a b)))

    (format t "Vector A: ") (print-vec3 a) (format t "~%")
    (format t "Vector B: ") (print-vec3 b) (format t "~%")
    (format t "A x B (cross): ") (print-vec3 cross) (format t "~%")
    (format t "A . B (dot): ~,3F~%" dot))

  ;; Normalization
  (format t "~%--- Normalization ---~%~%")

  (let* ((v (glk-vector3-make 3.0 4.0 0.0))
         (len (glk-vector3-length v))
         (normalized (glk-vector3-normalize v)))

    (format t "Vector V: ") (print-vec3 v) (format t "~%")
    (format t "Length: ~,3F~%" len)
    (format t "Normalized: ") (print-vec3 normalized) (format t "~%")
    (format t "Normalized length: ~,3F~%" (glk-vector3-length normalized)))

  ;; Matrix operations (64 bytes - hidden pointer return)
  (format t "~%--- Matrix Operations (64 bytes each) ---~%~%")

  (let* ((translate (glk-matrix4-translation 10.0 20.0 30.0))
         (rotate (glk-matrix4-rotation-y (/ pi 4)))  ; 45 degrees
         (scale (glk-matrix4-scale 2.0 2.0 2.0)))

    (format t "Translation matrix (10, 20, 30):")
    (print-matrix4 translate)

    (format t "~%Y-Rotation matrix (45 degrees):")
    (print-matrix4 rotate)

    ;; Combine matrices
    (let ((combined (glk-matrix4-multiply translate rotate)))
      (format t "~%Combined (translate * rotate):")
      (print-matrix4 combined)))

  ;; Camera matrices
  (format t "~%--- Camera Matrices ---~%~%")

  (let ((view (glk-matrix4-look-at
               0.0 5.0 10.0    ; eye position
               0.0 0.0 0.0     ; look at origin
               0.0 1.0 0.0))   ; up vector
        (proj (glk-matrix4-perspective
               (/ pi 4)        ; 45 degree FOV
               1.7778          ; 16:9 aspect
               0.1             ; near plane
               100.0)))        ; far plane

    (format t "View matrix (look at origin from (0,5,10)):")
    (print-matrix4 view)

    (format t "~%Perspective projection (45 deg FOV, 16:9):")
    (print-matrix4 proj))

  ;; Quaternion operations
  (format t "~%--- Quaternion Operations ---~%~%")

  (let* ((q1 (glk-quaternion-angle-axis (/ pi 4) 0.0 1.0 0.0))  ; 45 deg Y
         (q2 (glk-quaternion-angle-axis (/ pi 4) 1.0 0.0 0.0))  ; 45 deg X
         (combined (glk-quaternion-multiply q1 q2))
         (point (glk-vector3-make 1.0 0.0 0.0))
         (rotated (glk-quaternion-rotate-vector3 q1 point)))

    (format t "Quaternion q1 (45 deg Y): ")
    (print-vec4 q1) (format t "~%")

    (format t "Quaternion q2 (45 deg X): ")
    (print-vec4 q2) (format t "~%")

    (format t "Combined q1*q2: ")
    (print-vec4 combined) (format t "~%")

    (format t "~%Point (1,0,0) rotated by q1: ")
    (print-vec3 rotated) (format t "~%"))

  ;; Transform a point through the pipeline
  (format t "~%--- Transform Pipeline ---~%~%")

  (let* ((model (glk-matrix4-multiply
                 (glk-matrix4-translation 5.0 0.0 0.0)
                 (glk-matrix4-rotation-y (/ pi 2))))
         (view (glk-matrix4-look-at
                0.0 0.0 20.0
                0.0 0.0 0.0
                0.0 1.0 0.0))
         (proj (glk-matrix4-perspective (/ pi 4) 1.0 0.1 100.0))
         (mv (glk-matrix4-multiply view model))
         (mvp (glk-matrix4-multiply proj mv))
         (point (glk-vector4-make 0.0 0.0 0.0 1.0))
         (transformed (glk-matrix4-mul-vector4 mvp point)))

    (format t "Model: translate(5,0,0) * rotateY(90)~%")
    (format t "View: look at origin from (0,0,20)~%")
    (format t "Projection: 45 deg FOV~%")
    (format t "~%Point (0,0,0) in model space~%")
    (format t "After MVP transform: ")
    (print-vec4 transformed)
    (format t "~%")
    ;; Perspective divide
    (let ((w (slot transformed 'w)))
      (unless (zerop w)
        (format t "After perspective divide (NDC): (~,3F, ~,3F, ~,3F)~%"
                (/ (slot transformed 'x) w)
                (/ (slot transformed 'y) w)
                (/ (slot transformed 'z) w)))))

  (format t "~%========================================================~%")
  (format t "                   Demo Complete~%")
  (format t "========================================================~%~%"))

;;; Entry point
;; (glkit-demo)
