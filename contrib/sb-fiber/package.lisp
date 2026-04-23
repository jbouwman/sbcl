;;;; -*-  Lisp -*-
;;;;
;;;; Fiber (stackful coroutine) support for SBCL.

(defpackage :sb-fiber
  (:use :cl :sb-alien :sb-ext)
  (:export
   #:fiber
   #:make-fiber
   #:make-main-fiber
   #:fiber-switch
   #:fiber-alive-p
   #:fiber-state
   #:destroy-fiber
   #:current-fiber
   #:with-fiber
   #:+fiber-new+
   #:+fiber-runnable+
   #:+fiber-running+
   #:+fiber-dead+))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (sb-int:system-package-p (find-package "SB-FIBER")) t))
