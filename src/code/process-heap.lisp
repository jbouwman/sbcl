;;;; Per-process heaps: the core-level primitives.  The public API and
;;;; the fiber integration live in contrib/sb-fiber.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

#+sb-process-heaps
(progn

(define-alien-routine ("process_heap_owner_of" %process-heap-owner-of)
    (unsigned 32)
  (object unsigned))

(define-alien-routine ("process_heap_check_store" %process-heap-check-store)
    void
  (value unsigned-long)
  (object unsigned-long))

(defun object-owner (object)
  "Return the id of the process heap holding OBJECT, or 0 if OBJECT is
immediate or lives in the global heap."
  (if (sb-int:fixnump object)
      0
      (with-pinned-objects (object)
        (%process-heap-owner-of (get-lisp-obj-address object)))))

(defun check-process-heap-store (object value)
  "Apply the active process-heap store policy to storing VALUE in OBJECT."
  (with-pinned-objects (object value)
    (%process-heap-check-store (get-lisp-obj-address value)
                               (get-lisp-obj-address object))))

(defun process-owned-p (object)
  (/= 0 (object-owner object)))

;;; The switching primitives deal in raw addresses (fixnums) rather than
;;; SAPs so that they never allocate: they run in cleanup forms, and
;;; while an exhausted heap is installed nothing can be allocated.
(define-alien-routine ("process_heap_current_address" current-process-heap-address)
    unsigned-long)

(defun current-process-heap-sap ()
  (int-sap (current-process-heap-address)))

(declaim (inline process-heap-active-p))
(defun process-heap-active-p ()
  (/= 0 (current-process-heap-address)))

;;; Allocate a standard instance in the installed process heap.  PCL's own
;;; ALLOCATE-STANDARD-INSTANCE is compiled with the system TLAB forced and
;;; calls this when a process heap is active.
(defun allocate-process-instance (layout nslots unbound-marker)
  (declare (type sb-kernel:layout layout) (type index nslots))
  (let ((instance (sb-kernel::%new-instance layout (1+ instance-data-start)))
        (slots (make-array nslots :initial-element unbound-marker)))
    (sb-kernel:%instance-set instance instance-data-start slots)
    instance))

(define-alien-routine ("process_heap_switch_address" %switch-process-heap)
    int
  (heap unsigned-long))

(defun sb-kernel::call-with-global-heap (thunk)
  (declare (function thunk) (dynamic-extent thunk))
  (let ((prev (current-process-heap-address)))
    (if (zerop prev)
        (funcall thunk)
        (progn
          (%switch-process-heap 0)
          (unwind-protect (funcall thunk)
            (%switch-process-heap prev))))))

(define-alien-routine ("process_heap_collect" %process-heap-collect) int
  (heap system-area-pointer)
  (full int))

(defun process-heap-collect (sap full)
  "Collect the process heap SAP, which must be installed on the current
thread: the young generation only, or everything if FULL. Returns 0 on
success."
  (without-gcing (%process-heap-collect sap (if full 1 0))))

;;; Called from C (interrupt_handle_pending) when an allocation slow path
;;; asked for a local collection of the current heap.
(defun process-heap-collect-pending ()
  (without-gcing
    (alien-funcall (extern-alien "process_heap_collect_pending" (function int))))
  nil)

(define-condition sb-kernel::process-heap-exhausted-error (storage-condition)
  ((available :initarg :available :reader sb-kernel::process-heap-exhausted-error-available-bytes)
   (requested :initarg :requested :reader sb-kernel::process-heap-exhausted-error-requested-bytes))
  (:report
   (lambda (condition stream)
     (format stream "Process heap exhausted (hard limit reached).
~D bytes available, ~D requested."
             (sb-kernel::process-heap-exhausted-error-available-bytes condition)
             (sb-kernel::process-heap-exhausted-error-requested-bytes condition)))))

(define-alien-routine ("process_heap_take_exhausted" %take-exhausted-process-heap)
    unsigned-long)

;;; --- Store barrier ---

(define-condition sb-kernel::process-heap-store-error (error)
  ((object :initarg :object :reader sb-kernel::process-heap-store-error-object)
   (value :initarg :value :reader sb-kernel::process-heap-store-error-value)
   (kind :initarg :kind :reader sb-kernel::process-heap-store-error-kind))
  (:report
   (lambda (condition stream)
     (let ((object (sb-kernel::process-heap-store-error-object condition))
           (value (sb-kernel::process-heap-store-error-value condition)))
       (ecase (sb-kernel::process-heap-store-error-kind condition)
         (:escape
          (format stream "Storing ~S, which is owned by a process heap, ~
                          into the global object ~S." value object))
         (:cross-heap
          (format stream "Storing ~S into ~S, which is owned by a different ~
                          process heap." value object))
         (:global
          (format stream "Mutating the global object ~S from a strict process ~
                          heap (storing ~S)." object value)))))))

;;; The thread slot read by the store barrier: the installed heap when
;;; its stores are checked, else 0.
(defun %store-check-suspend ()
  (prog1 (sap-int (current-thread-offset-sap thread-process-heap-check-slot))
    (setf (sap-ref-word (sb-thread:current-thread-sap)
                        (ash thread-process-heap-check-slot word-shift))
          0)))

(defun %store-check-resume (saved)
  (setf (sap-ref-word (sb-thread:current-thread-sap)
                      (ash thread-process-heap-check-slot word-shift))
        saved))

;;; Called from C (process_heap_check_store) for a store that violates
;;; the ownership rules.  Checking is suspended while the error is
;;; handled so that the handlers' own stores do not recurse into it.
(defun sb-kernel::process-heap-store-error (object value kind)
  (let ((saved (%store-check-suspend)))
    (unwind-protect
         (cerror "Perform the store anyway."
                 'sb-kernel::process-heap-store-error
                 :object object :value value
                 :kind (ecase kind (1 :escape) (2 :cross-heap) (3 :global)))
      (%store-check-resume saved))))

;;; Called from C (process_heap_exhausted).  The runtime has already
;;; uninstalled the exhausted heap, so the condition is built in the
;;; global heap; reinstall the heap when the error is handled within its
;;; dynamic extent.
(defun sb-kernel::process-heap-exhausted-error (available requested)
  (declare (fixnum available requested))
  (let ((heap (%take-exhausted-process-heap)))
    (unwind-protect
         (sb-kernel::infinite-error-protect
          (error 'sb-kernel::process-heap-exhausted-error
                 :available (ash available n-fixnum-tag-bits)
                 :requested (ash requested n-fixnum-tag-bits)))
      (unless (zerop heap)
        (%switch-process-heap heap)))))

) ; end PROGN
