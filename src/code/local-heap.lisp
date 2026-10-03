;;;; Per-local heaps: the core-level primitives.  The public API and
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

#+sb-local-heaps
(progn

(define-alien-routine ("local_heap_owner_of" %local-heap-owner-of)
    (unsigned 32)
  (object unsigned))

(defun object-owner (object)
  "Return the id of the local heap holding OBJECT, or 0 if OBJECT is
immediate or lives in the global heap."
  (if (sb-int:fixnump object)
      0
      (with-pinned-objects (object)
        (%local-heap-owner-of (get-lisp-obj-address object)))))

(defun locally-owned-p (object)
  (/= 0 (object-owner object)))

;;; The switching primitives deal in raw addresses (fixnums) rather than
;;; SAPs so that they never allocate: they run in cleanup forms, and
;;; while an exhausted heap is installed nothing can be allocated.
(define-alien-routine ("local_heap_current_address" current-local-heap-address)
    unsigned-long)

(defun current-local-heap-sap ()
  (int-sap (current-local-heap-address)))

(declaim (inline local-heap-active-p))
(defun local-heap-active-p ()
  (/= 0 (current-local-heap-address)))

;;; Allocate a standard instance in the installed local heap.  PCL's own
;;; ALLOCATE-STANDARD-INSTANCE is compiled with the system TLAB forced and
;;; calls this when a local heap is active.
(defun allocate-process-instance (layout nslots unbound-marker)
  (declare (type sb-kernel:layout layout) (type index nslots))
  (let ((instance (sb-kernel::%new-instance layout (1+ instance-data-start)))
        (slots (make-array nslots :initial-element unbound-marker)))
    (sb-kernel:%instance-set instance instance-data-start slots)
    instance))

(define-alien-routine ("local_heap_switch_address" %switch-local-heap)
    int
  (heap unsigned-long))

(define-alien-routine ("local_heap_install_global" %install-global-heap) int)

;;; Run THUNK with the global heap installed on the current heap's behalf:
;;; what it allocates is global, the strict rule is off, and a store that
;;; would make a global object refer into a local heap is still refused.
;;; The checking state in effect on entry, suspended or not, is in effect
;;; inside and restored on exit: reinstalling the heap would otherwise set
;;; it from the heap's mode and end a suspension around this form.
(defun sb-kernel::call-with-global-heap (thunk)
  (declare (function thunk) (dynamic-extent thunk))
  (let ((prev (current-local-heap-address)))
    (if (zerop prev)
        (funcall thunk)
        (let ((check (sap-int (current-thread-offset-sap thread-local-heap-check-slot))))
          (%install-global-heap)
          (unwind-protect (funcall thunk)
            (%switch-local-heap prev)
            (%store-check-resume check))))))

(define-alien-routine ("local_heap_from_id" %local-heap-from-id) unsigned-long
  (id (unsigned 32)))

;;; Call THUNK with the heap that owns OBJECT installed, and return its
;;; value and T.  Return NIL and NIL without calling THUNK when OBJECT is
;;; global or its heap is installed on another thread.
(defun sb-kernel::call-with-heap-of (object thunk)
  (declare (function thunk) (dynamic-extent thunk))
  (let ((owner (object-owner object)))
    (if (zerop owner)
        (values nil nil)
        (let ((heap (%local-heap-from-id owner))
              (prev (current-local-heap-address))
              ;; Restored with PREV, as CALL-WITH-GLOBAL-HEAP restores it.
              (check (sap-int (current-thread-offset-sap thread-local-heap-check-slot))))
          (cond ((zerop heap) (values nil nil))
                ((= heap prev) (values (funcall thunk) t))
                ((zerop (%switch-local-heap heap))
                 (unwind-protect (values (funcall thunk) t)
                   (%switch-local-heap prev)
                   (%store-check-resume check)))
                (t (values nil nil)))))))

(define-alien-routine ("local_heap_collect" %local-heap-collect) int
  (heap system-area-pointer)
  (full int))

(defun local-heap-collect (sap full)
  "Collect the local heap SAP, which must be installed on the current
thread: the young generation only, or everything if FULL. Returns 0 on
success."
  (without-gcing (%local-heap-collect sap (if full 1 0))))

;;; Called from C (interrupt_handle_pending) when an allocation slow path
;;; asked for a local collection of the current heap.
(defun local-heap-collect-pending ()
  (without-gcing
    (alien-funcall (extern-alien "local_heap_collect_pending" (function int))))
  nil)

(define-condition sb-kernel::local-heap-exhausted-error (storage-condition)
  ((available :initarg :available :reader sb-kernel::local-heap-exhausted-error-available-bytes)
   (requested :initarg :requested :reader sb-kernel::local-heap-exhausted-error-requested-bytes))
  (:documentation "Signaled by an allocation that would take the installed heap past
its hard limit, with the global heap installed for the extent of the error;
the heap is reinstalled when the error is handled within its dynamic extent.
The readers give the bytes available and requested.")
  (:report
   (lambda (condition stream)
     (format stream "Local heap exhausted (hard limit reached).
~D bytes available, ~D requested."
             (sb-kernel::local-heap-exhausted-error-available-bytes condition)
             (sb-kernel::local-heap-exhausted-error-requested-bytes condition)))))

(define-alien-routine ("local_heap_take_exhausted" %take-exhausted-local-heap)
    unsigned-long)

;;; --- Allocation trap ---

(define-condition sb-kernel::heap-allocation-trap (condition)
  ((heap-id :initarg :heap-id :reader sb-kernel::heap-allocation-trap-heap-id)
   (heap-epoch :initarg :heap-epoch :reader sb-kernel::heap-allocation-trap-heap-epoch)
   (claimed :initarg :claimed :reader sb-kernel::heap-allocation-trap-claimed)
   (trap :initarg :trap :reader sb-kernel::heap-allocation-trap-trap))
  (:documentation "Signaled, through *HEAP-ALLOCATION-TRAP-FUNCTION* when it is set,
once the bytes a heap has ever claimed pass the threshold that
ARM-HEAP-ALLOCATION-TRAP set; the readers give the heap's id and epoch, the
bytes claimed and the threshold.")
  (:report
   (lambda (condition stream)
     (format stream "Local heap #~D claimed ~D bytes, past its allocation ~
                     trap at ~D."
             (sb-kernel::heap-allocation-trap-heap-id condition)
             (sb-kernel::heap-allocation-trap-claimed condition)
             (sb-kernel::heap-allocation-trap-trap condition)))))

(define-alien-routine ("local_heap_take_alloc_trap" take-local-heap-allocation-trap)
    unsigned-long)

(define-alien-routine ("local_heap_stat" %local-heap-stat) unsigned-long
  (heap unsigned-long)
  (which int))

(defvar sb-kernel::*heap-allocation-trap-function* nil)
(declaim (type (or function null) sb-kernel::*heap-allocation-trap-function*)
         (always-bound sb-kernel::*heap-allocation-trap-function*))

(defun signal-local-heap-allocation-trap (trap)
  (let* ((heap (current-local-heap-address))
         (condition (make-condition 'sb-kernel::heap-allocation-trap
                                    :heap-id (%local-heap-stat heap 11)
                                    :heap-epoch (%local-heap-stat heap 23)
                                    :claimed (%local-heap-stat heap 24)
                                    :trap trap))
         (function sb-kernel::*heap-allocation-trap-function*))
    (if function
        (funcall function condition)
        (signal condition))
    nil))

;;; --- Store barrier ---

(define-condition sb-kernel::heap-store-error (error)
  ((object :initarg :object :reader sb-kernel::heap-store-error-object)
   (value :initarg :value :reader sb-kernel::heap-store-error-value)
   (kind :initarg :kind :reader sb-kernel::heap-store-error-kind))
  (:documentation "Signaled by the store barrier, in the storing thread, before the
store is performed. HEAP-STORE-ERROR-OBJECT is the object stored into,
HEAP-STORE-ERROR-VALUE the value, and HEAP-STORE-ERROR-KIND :ESCAPE for a
locally-owned value stored into a global object, :CROSS-HEAP for a value owned
by another heap, or :GLOBAL for any checked store into a global object under a
strict heap. A CONTINUE restart performs the store anyway.")
  (:report
   (lambda (condition stream)
     (let ((object (sb-kernel::heap-store-error-object condition))
           (value (sb-kernel::heap-store-error-value condition)))
       (ecase (sb-kernel::heap-store-error-kind condition)
         (:escape
          (format stream "Storing ~S, which is owned by a local heap, ~
                          into the global object ~S." value object))
         (:cross-heap
          (format stream "Storing ~S into ~S, which is owned by a different ~
                          local heap." value object))
         (:global
          (format stream "Mutating the global object ~S from a strict process ~
                          heap (storing ~S)." object value)))))))

;;; The thread slot read by the store barrier: the installed heap when
;;; its stores are checked, else 0.
(defun %store-check-suspend ()
  (prog1 (sap-int (current-thread-offset-sap thread-local-heap-check-slot))
    (setf (sap-ref-word (sb-thread:current-thread-sap)
                        (ash thread-local-heap-check-slot word-shift))
          0)))

(defun %store-check-resume (saved)
  (setf (sap-ref-word (sb-thread:current-thread-sap)
                      (ash thread-local-heap-check-slot word-shift))
        saved))

(define-alien-routine ("local_heap_classify_store" %classify-store) int
  (value unsigned)
  (object unsigned)
  (pc unsigned))

;;; The store barrier's check, for a store of VALUE into OBJECT that the
;;; compiler does not barrier: the initializing stores of an object
;;; allocated in the system TLAB, which is global under a local heap, are
;;; stores of VALUE into a global object that no barrier sees.  PC is the
;;; return address to name as the storing site.  Records or signals
;;; exactly as the barrier would; a handler that continues lets the store
;;; proceed.
(defun check-store (object value pc)
  (declare (type word pc))
  (unless (zerop (sap-int (current-thread-offset-sap thread-local-heap-check-slot)))
    (let ((kind (with-pinned-objects (object value)
                  (%classify-store (get-lisp-obj-address value)
                                   (get-lisp-obj-address object)
                                   pc))))
      (unless (zerop kind)
        (sb-kernel::heap-store-error object value kind))))
  nil)

;;; Called from C (local_heap_check_store) for a store that violates
;;; the ownership rules.  Checking is suspended while the error is
;;; handled so that the handlers' own stores do not recurse into it.
(defun sb-kernel::heap-store-error (object value kind)
  (let ((saved (%store-check-suspend)))
    (unwind-protect
         (cerror "Perform the store anyway."
                 'sb-kernel::heap-store-error
                 :object object :value value
                 :kind (ecase kind (1 :escape) (2 :cross-heap) (3 :global)))
      (%store-check-resume saved))))

;;; Called from C (local_heap_exhausted).  The runtime has already
;;; uninstalled the exhausted heap, so the condition is built in the
;;; global heap; reinstall the heap when the error is handled within its
;;; dynamic extent.
(defun sb-kernel::local-heap-exhausted-error (available requested)
  (declare (fixnum available requested))
  (let ((heap (%take-exhausted-local-heap)))
    (unwind-protect
         (sb-kernel::infinite-error-protect
          (error 'sb-kernel::local-heap-exhausted-error
                 :available (ash available n-fixnum-tag-bits)
                 :requested (ash requested n-fixnum-tag-bits)))
      (unless (zerop heap)
        (%switch-local-heap heap)))))

) ; end PROGN
