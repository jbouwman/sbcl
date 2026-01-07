;;;; objc-bridge.lisp - Minimal Objective-C Runtime Bridge
;;;;
;;;; A lightweight bridge to the Objective-C runtime for calling
;;;; framework methods that return structs by value.
;;;;
;;;; Requires: macOS with libobjc
;;;; Uses: struct-by-value returns (srbv branch feature)

(defpackage #:objc-bridge
  (:use #:cl)
  (:export
   ;; Core runtime
   #:objc-class
   #:objc-selector
   #:objc-msg-send
   #:objc-msg-send-stret
   ;; Convenience
   #:@selector
   #:@class
   ;; NSValue struct extraction
   #:nsvalue-point
   #:nsvalue-size
   #:nsvalue-rect))

(in-package #:objc-bridge)

;;; ============================================================================
;;; Load Objective-C Runtime
;;; ============================================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-case
      (load-shared-object "/usr/lib/libobjc.A.dylib")
    (error (c)
      (warn "Could not load Objective-C runtime: ~A~%This demo requires macOS." c))))

;;; ============================================================================
;;; Basic Types
;;; ============================================================================

;;; Objective-C id type (pointer to object)
(define-alien-type objc-id (* t))

;;; Objective-C Class type
(define-alien-type objc-class (* t))

;;; Objective-C SEL type (selector/method name)
(define-alien-type objc-sel (* t))

;;; Objective-C IMP type (method implementation pointer)
(define-alien-type objc-imp (* t))

;;; ============================================================================
;;; Runtime Functions
;;; ============================================================================

(define-alien-routine ("objc_getClass" %objc-get-class) objc-class
  (name c-string))

(define-alien-routine ("sel_registerName" %sel-register-name) objc-sel
  (name c-string))

(define-alien-routine ("class_getName" %class-get-name) c-string
  (cls objc-class))

(define-alien-routine ("sel_getName" %sel-get-name) c-string
  (sel objc-sel))

;;; objc_msgSend is special - it's a trampoline with variable signature
;;; We'll define typed wrappers for specific return types

;;; Generic pointer return (most common case)
(define-alien-routine ("objc_msgSend" %msg-send-ptr) (* t)
  (receiver objc-id)
  (selector objc-sel)
  &rest)

;;; Double return (for CGFloat on 64-bit)
(define-alien-routine ("objc_msgSend" %msg-send-double) double
  (receiver objc-id)
  (selector objc-sel)
  &rest)

;;; Integer return
(define-alien-routine ("objc_msgSend" %msg-send-int) long
  (receiver objc-id)
  (selector objc-sel)
  &rest)

;;; ============================================================================
;;; Core Graphics Types (for NSValue extraction)
;;; ============================================================================

(define-alien-type cg-float double)

(define-alien-type cg-point
    (struct cg-point
      (x cg-float)
      (y cg-float)))

(define-alien-type cg-size
    (struct cg-size
      (width cg-float)
      (height cg-float)))

(define-alien-type cg-rect
    (struct cg-rect
      (origin cg-point)
      (size cg-size)))

;;; ============================================================================
;;; Struct-returning message sends
;;; ============================================================================

;;; For small structs (<=16 bytes) that can be returned in registers,
;;; objc_msgSend works directly. For larger structs, we need objc_msgSend_stret.

;;; On ARM64, objc_msgSend_stret doesn't exist - all struct returns go through
;;; objc_msgSend with the hidden pointer in x8.

;;; On x86-64, objc_msgSend_stret is used for structs > 16 bytes.

;;; CGPoint return (16 bytes - fits in registers on both platforms)
(define-alien-routine ("objc_msgSend" %msg-send-point) cg-point
  (receiver objc-id)
  (selector objc-sel)
  &rest)

;;; CGSize return (16 bytes - fits in registers)
(define-alien-routine ("objc_msgSend" %msg-send-size) cg-size
  (receiver objc-id)
  (selector objc-sel)
  &rest)

;;; CGRect return (32 bytes - needs stret on x86-64)
#+x86-64
(define-alien-routine ("objc_msgSend_stret" %msg-send-rect) cg-rect
  (receiver objc-id)
  (selector objc-sel)
  &rest)

#+arm64
(define-alien-routine ("objc_msgSend" %msg-send-rect) cg-rect
  (receiver objc-id)
  (selector objc-sel)
  &rest)

;;; ============================================================================
;;; High-level Interface
;;; ============================================================================

(defun objc-class (name)
  "Get an Objective-C class by name (string)."
  (let ((cls (%objc-get-class name)))
    (when (sb-alien:null-alien cls)
      (error "Objective-C class not found: ~A" name))
    cls))

(defun objc-selector (name)
  "Get an Objective-C selector by name (string)."
  (%sel-register-name name))

(defmacro @class (name)
  "Convenience macro for getting a class at compile time."
  `(load-time-value (objc-class ,name)))

(defmacro @selector (name)
  "Convenience macro for getting a selector at compile time."
  `(load-time-value (objc-selector ,name)))

;;; ============================================================================
;;; NSValue Struct Extraction
;;; ============================================================================

;;; NSValue can wrap C structs. These methods extract common geometry types.

(defun nsvalue-point (nsvalue)
  "Extract a CGPoint from an NSValue."
  (%msg-send-point nsvalue (@selector "pointValue")))

(defun nsvalue-size (nsvalue)
  "Extract a CGSize from an NSValue."
  (%msg-send-size nsvalue (@selector "sizeValue")))

(defun nsvalue-rect (nsvalue)
  "Extract a CGRect from an NSValue."
  (%msg-send-rect nsvalue (@selector "rectValue")))

;;; ============================================================================
;;; Message Send Convenience
;;; ============================================================================

(defun msg-send (receiver selector &rest args)
  "Send a message returning a pointer/object."
  (apply #'%msg-send-ptr receiver selector args))

(defun msg-send-int (receiver selector &rest args)
  "Send a message returning an integer."
  (apply #'%msg-send-int receiver selector args))

(defun msg-send-double (receiver selector &rest args)
  "Send a message returning a double."
  (apply #'%msg-send-double receiver selector args))

;;; ============================================================================
;;; Example Usage
;;; ============================================================================

(defun demo ()
  "Demonstrate the Objective-C bridge."
  (format t "~%Objective-C Bridge Demo~%")
  (format t "========================~%~%")

  ;; Get some class info
  (let ((ns-object (@class "NSObject"))
        (ns-string (@class "NSString"))
        (ns-value (@class "NSValue")))

    (format t "Found classes:~%")
    (format t "  NSObject: ~A~%" ns-object)
    (format t "  NSString: ~A~%" ns-string)
    (format t "  NSValue:  ~A~%~%" ns-value)

    ;; Create an NSValue containing a CGPoint
    ;; [NSValue valueWithPoint:NSMakePoint(100, 200)]
    ;; Note: This is a simplified example - real usage would need more setup

    (format t "The bridge demonstrates:~%")
    (format t "  - objc_msgSend for small struct returns (CGPoint, CGSize)~%")
    (format t "  - objc_msgSend_stret for large struct returns (CGRect on x86-64)~%")
    (format t "  - ARM64 uses objc_msgSend for all struct returns (via x8 register)~%~%")

    (format t "Struct return conventions:~%")
    (format t "  CGPoint (16 bytes): Register return on both platforms~%")
    (format t "  CGSize  (16 bytes): Register return on both platforms~%")
    (format t "  CGRect  (32 bytes): ~%")
    (format t "    x86-64: objc_msgSend_stret (hidden first arg in RDI)~%")
    (format t "    ARM64:  objc_msgSend (hidden pointer in x8)~%")))

;;; ============================================================================
;;; Notes
;;; ============================================================================
#|
This is a minimal Objective-C bridge focused on demonstrating struct-by-value
returns. A production bridge would need:

1. Object lifetime management (autorelease pools, reference counting)
2. More complete type encoding support
3. Block (closure) support
4. Exception handling bridge
5. Method swizzling utilities
6. Property access macros

The key struct-by-value considerations for Objective-C:

1. objc_msgSend is polymorphic - its calling convention depends on the
   method being called. The compiler generates different calls based on
   the expected return type.

2. On x86-64:
   - Structs <= 16 bytes: returned in registers (RAX/RDX or XMM0/XMM1)
   - Structs > 16 bytes: use objc_msgSend_stret, hidden pointer in RDI

3. On ARM64:
   - All struct returns use objc_msgSend
   - Structs <= 16 bytes: returned in x0/x1 or via HFA in floating registers
   - Structs > 16 bytes: hidden pointer passed in x8

4. The ObjC runtime also has:
   - objc_msgSend_fpret: for floating-point returns on x86
   - objc_msgSendSuper: for calling superclass methods
   - objc_msgSendSuper_stret: combination of above

SBCL's struct-by-value support makes this much cleaner than the traditional
approach of manually allocating buffers and using pointer tricks.
|#
