;;;; -*-  Lisp -*-
;;;;
;;;; Per-process heaps: Lisp API, message copying, and mailboxes.
;;;;
;;;; A heap is a GC ownership domain inside dynamic space.  Objects
;;;; allocated while a heap is installed on the current thread belong
;;;; to it; they may refer to global objects and to each other, but
;;;; nothing global and no other heap may refer to them.  Under that
;;;; invariant a heap can be collected on its own (HEAP-GC) and freed
;;;; wholesale (RELEASE-HEAP), and objects are moved between heaps by
;;;; copying (SEND-MESSAGE / RECEIVE-MESSAGE).

(in-package :sb-fiber)

#+sb-process-heaps
(progn

;;; --- Runtime bindings ---

(define-alien-routine ("process_heap_create" %heap-create) system-area-pointer
  (kind int)
  (gc-threshold unsigned-long)
  (hard-limit unsigned-long)
  (flags int))

(define-alien-routine ("process_heap_set_flags" %heap-set-flags) void
  (heap system-area-pointer)
  (flags int))

(define-alien-routine ("process_heap_set_fullsweep_after" %heap-set-fullsweep-after) void
  (heap system-area-pointer)
  (count unsigned-long))

(define-alien-routine ("process_heap_release" %heap-release) int
  (heap system-area-pointer))

(define-alien-routine ("process_heap_stat" %heap-stat) unsigned-long
  (heap system-area-pointer)
  (which int))

;;; Entry points for threads other than the owner take the heap's id and
;;; epoch and look it up under the runtime's table lock: a released
;;; heap's descriptor is gone and its id may name a newer heap.
(define-alien-routine ("process_heap_stat_id" %heap-stat-id) unsigned-long
  (id (unsigned 32))
  (epoch (unsigned 32))
  (which int))

(define-alien-routine ("process_heap_release_id" %heap-release-id) int
  (id (unsigned 32))
  (epoch (unsigned 32)))

(define-alien-routine ("process_heap_send_id" %heap-send-id) int
  (dest-id (unsigned 32))
  (dest-epoch (unsigned 32))
  (fragment system-area-pointer)
  (sender (unsigned 32)))

(define-alien-routine ("process_heap_verify" %heap-verify) int
  (heap system-area-pointer))

(define-alien-routine ("process_heap_violation_count" %heap-violation-count) int)
(define-alien-routine ("process_heap_reset_violations" %heap-reset-violations) void)

(define-alien-routine ("process_heap_seal" %heap-seal) int
  (fragment system-area-pointer))

(define-alien-routine ("process_heap_send" %heap-send) int
  (dest system-area-pointer)
  (fragment system-area-pointer)
  (sender (unsigned 32)))

(defconstant +heap-kind-process+ 1)
(defconstant +heap-kind-fragment+ 2)

(defconstant +stat-bytes-allocated+ 0)
(defconstant +stat-bytes-live+ 1)
(defconstant +stat-bytes-since-gc+ 2)
(defconstant +stat-gc-count+ 3)
(defconstant +stat-page-count+ 4)
(defconstant +stat-block-count+ 19)
(defconstant +stat-outgoing-count+ 20)
(defconstant +stat-global-root-count+ 21)
(defconstant +stat-concurrency-peak+ 22)
(defconstant +stat-epoch+ 23)
(defconstant +stat-mailbox-count+ 5)
(defconstant +stat-mailbox-bytes+ 6)
(defconstant +stat-gc-time+ 7)
(defconstant +stat-gc-threshold+ 8)
(defconstant +stat-hard-limit+ 9)
(defconstant +stat-state+ 10)
(defconstant +stat-id+ 11)
(defconstant +stat-store-check+ 13)
(defconstant +stat-strict+ 14)
(defconstant +stat-minor-gc-count+ 15)
(defconstant +stat-major-gc-count+ 16)
(defconstant +stat-bytes-old+ 17)
(defconstant +stat-fullsweep-after+ 18)

(defconstant +flag-record-stores+ 1)
(defconstant +flag-signal-stores+ 2)
(defconstant +flag-strict+ 4)

(defun %heap-flags (check-stores strict)
  (logior (ecase check-stores
            ((nil) 0)
            (:record +flag-record-stores+)
            (:error +flag-signal-stores+))
          (if strict +flag-strict+ 0)))

;;; --- Conditions ---

(define-condition heap-error (error)
  ((heap :initarg :heap :reader heap-error-heap :initform nil))
  (:documentation "Base class for sb-fiber heap errors."))

(define-condition dead-heap-error (heap-error) ()
  (:report (lambda (c stream)
             (format stream "heap ~S has been released" (heap-error-heap c)))))

(define-condition heap-in-use-error (heap-error) ()
  (:report (lambda (c stream)
             (format stream "heap ~S is installed on another thread"
                     (heap-error-heap c)))))

(define-condition heap-not-current-error (heap-error) ()
  (:report (lambda (c stream)
             (format stream "heap ~S is not the current heap of this thread"
                     (heap-error-heap c)))))

(define-condition no-current-heap-error (heap-error)
  ((operation :initarg :operation :initform nil
              :reader no-current-heap-error-operation))
  (:report (lambda (c stream)
             (format stream "~@[~(~A~): ~]no heap is installed on this thread"
                     (no-current-heap-error-operation c)))))

(define-condition untransferable-object (heap-error)
  ((object :initarg :object :reader untransferable-object-object))
  (:documentation
   "COPY-FOR-TRANSFER met an object that cannot be copied into another heap.")
  (:report (lambda (c stream)
             (format stream "~S cannot be transferred between heaps"
                     (untransferable-object-object c)))))

(define-condition cross-heap-reference (heap-error)
  ((object :initarg :object :reader cross-heap-reference-object))
  (:documentation
   "An object owned by a process heap was about to be stored where only
global objects may live.")
  (:report (lambda (c stream)
             (format stream "~S is owned by a process heap and cannot be ~
                             referenced from the global heap"
                     (cross-heap-reference-object c)))))

;;; --- The heap object ---

(defstruct (heap (:constructor %make-heap)
                 (:print-object %print-heap))
  (sap (sb-sys:int-sap 0) :type sb-sys:system-area-pointer)
  (id 0 :type (unsigned-byte 32))
  (epoch 0 :type (unsigned-byte 32))
  (name nil :type (or null string))
  (fiber nil)
  (released-p nil :type boolean))

(defun %print-heap (heap stream)
  (print-unreadable-object (heap stream :type t :identity t)
    (format stream "~@[~A ~]#~D~:[~; released~]"
            (heap-name heap) (heap-id heap) (heap-released-p heap))))

(defvar *default-heap-gc-threshold* (* 4 1024 1024)
  "Bytes a heap may allocate past its live size before it is collected
automatically. Applies to heaps created without an explicit threshold.")

;;; Every live heap, by id.  Global: only touched with the global heap
;;; installed, so its storage never lands in a process heap.
(define-load-time-global *heaps* (make-hash-table :synchronized t))

(declaim (inline current-heap-address heap-active-p))
(defun current-heap-address ()
  (sb-vm::current-process-heap-address))
(defun heap-active-p ()
  (/= 0 (current-heap-address)))

;;; ADDRESS is a raw heap address (0 for the global heap).  Must not
;;; allocate: it runs in cleanups while an exhausted heap may be installed.
(defun %switch-heap (address &optional heap)
  (declare (type sb-vm:word address))
  (let ((rc (sb-vm::%switch-process-heap address)))
    (case rc
      (0 t)
      (-1 (error 'heap-in-use-error :heap heap))
      (-2 (error 'dead-heap-error :heap heap))
      (t (error "unexpected return code ~D switching heaps" rc)))))

(defun heap-sap-or-lose (heap)
  (declare (type heap heap))
  (when (heap-released-p heap)
    (error 'dead-heap-error :heap heap))
  (heap-sap heap))

(defmacro without-heap (&body body)
  "Execute BODY with the global heap installed, so that everything it
allocates is shared rather than owned by the current process heap.
Restores the previous heap on exit."
  (let ((prev (gensym "PREV")))
    `(let ((,prev (current-heap-address)))
       (if (zerop ,prev)
           (progn ,@body)
           (progn
             (%switch-heap 0)
             (unwind-protect (progn ,@body)
               (%switch-heap ,prev)))))))

(defun call-with-heap (heap thunk)
  (declare (function thunk) (dynamic-extent thunk))
  (let ((prev (current-heap-address))
        (address (sb-sys:sap-int (heap-sap-or-lose heap))))
    (%switch-heap address heap)
    (unwind-protect (funcall thunk)
      (%switch-heap prev))))

(defmacro with-heap ((heap) &body body)
  "Execute BODY with HEAP installed as the allocation target of the
current thread. Restores the previous heap on exit."
  ;; The thunk must be stack-allocated: a heap-allocated closure would
  ;; put any variable BODY assigns into a global value cell, which the
  ;; heap's own collector does not treat as a root.
  (let ((thunk (gensym "WITH-HEAP-BODY")))
    `(sb-int:dx-flet ((,thunk () ,@body))
       (call-with-heap ,heap #',thunk))))

(defun make-heap (&key name
                       (gc-threshold *default-heap-gc-threshold*)
                       (hard-limit 0)
                       (fullsweep-after 16)
                       (check-stores :error)
                       strict)
  "Create a process heap.  Objects allocated while it is installed (see
WITH-HEAP and MAKE-FIBER's :HEAP argument) belong to it.  GC-THRESHOLD
is the number of bytes the heap may allocate before its young
generation is collected automatically; 0 disables automatic
collection.  FULLSWEEP-AFTER is the number of minor collections after
which the next collection is a full one; 0 leaves full collections to
HEAP-GC :FULL and to the old generation doubling in size.  HARD-LIMIT
bounds the heap's total size; 0 means unbounded.

CHECK-STORES controls the store barrier while the heap is installed:
:ERROR (the default) signals PROCESS-HEAP-STORE-ERROR when a pointer
store would make a global object or another heap refer into this heap,
:RECORD only records such stores (see HEAP-VIOLATIONS), NIL does not
check.  STRICT additionally treats any store into a global object as a
violation, so that a process can only mutate its own data."
  (declare (type (integer 0) gc-threshold hard-limit fullsweep-after)
           (type (member nil :record :error) check-stores))
  (without-heap
    (let ((sap (%heap-create +heap-kind-process+ gc-threshold hard-limit
                             (%heap-flags check-stores strict))))
      (when (zerop (sb-sys:sap-int sap))
        (error "failed to allocate a process heap"))
      (%heap-set-fullsweep-after sap fullsweep-after)
      (let ((heap (%make-heap :sap sap
                              :id (%heap-stat sap +stat-id+)
                              :epoch (%heap-stat sap +stat-epoch+)
                              :name (and name (copy-seq (string name))))))
        (setf (gethash (heap-id heap) *heaps*) heap)
        heap))))

(defun release-heap (heap)
  "Return every page of HEAP to the global free pool, along with any
queued messages.  Objects that were in HEAP must not be used afterwards.
HEAP must not be installed on another thread."
  (declare (type heap heap))
  (unless (heap-released-p heap)
    (let ((sap (heap-sap heap)))
      (when (= (current-heap-address) (sb-sys:sap-int sap))
        (%switch-heap 0))
      (without-heap
        (let ((rc (%heap-release-id (heap-id heap) (heap-epoch heap))))
          (case rc
            (0)
            (-1 (error 'heap-in-use-error :heap heap))
            (-2)
            (t (error "unexpected return code ~D releasing heap" rc))))
        (setf (heap-released-p heap) t
              (heap-sap heap) (sb-sys:int-sap 0))
        (remhash (heap-id heap) *heaps*))))
  heap)

(defun %heap-from-id (id)
  (if (zerop id) nil (gethash id *heaps*)))

(defun current-heap ()
  "The heap installed on the current thread, or NIL."
  (let ((addr (current-heap-address)))
    (if (zerop addr)
        nil
        (%heap-from-id (%heap-stat (sb-sys:int-sap addr) +stat-id+)))))

(defun object-heap (object)
  "The heap that owns OBJECT, or NIL if OBJECT is immediate or global."
  (%heap-from-id (sb-vm::object-owner object)))

(defun heap-alive-p (heap)
  (declare (type heap heap))
  (not (heap-released-p heap)))

(macrolet ((def (name which doc)
             `(defun ,name (heap)
                ,doc
                (declare (type heap heap))
                (if (heap-released-p heap)
                    0
                    (%heap-stat-id (heap-id heap) (heap-epoch heap) ,which)))))
  (def heap-bytes-allocated +stat-bytes-allocated+
    "Bytes of dynamic space currently claimed by HEAP.")
  (def heap-bytes-live +stat-bytes-live+
    "Bytes retained by HEAP's most recent collection.")
  (def heap-bytes-since-gc +stat-bytes-since-gc+
    "Bytes claimed by HEAP since its most recent collection.")
  (def heap-gc-count +stat-gc-count+
    "Number of local collections HEAP has undergone.")
  (def heap-minor-gc-count +stat-minor-gc-count+
    "Number of minor (young generation) collections of HEAP.")
  (def heap-major-gc-count +stat-major-gc-count+
    "Number of full collections of HEAP.")
  (def heap-bytes-old +stat-bytes-old+
    "Bytes in HEAP's old generation after its most recent collection.")
  (def heap-fullsweep-after +stat-fullsweep-after+
    "Number of minor collections of HEAP after which a full one is due;
0 means only explicit or growth-driven full collections. Setfable.")
  (def heap-page-count +stat-page-count+
    "Number of GC pages on which HEAP owns at least one block.")
  (def heap-block-count +stat-block-count+
    "Number of 4 KiB blocks owned by HEAP, large objects included.")
  (def heap-outgoing-count +stat-outgoing-count+
    "Number of global objects in HEAP's outgoing root summary: those its
old generation referred to when it was last traced.")
  (def heap-global-root-count +stat-global-root-count+
    "Number of HEAP's objects the most recent global collection walked as
roots: its young generation and the old objects on marked cards.")
  (def heap-gc-run-time +stat-gc-time+
    "Microseconds spent collecting HEAP.")
  (def heap-mailbox-count +stat-mailbox-count+
    "Number of messages waiting in HEAP's mailbox.")
  (def heap-mailbox-bytes +stat-mailbox-bytes+
    "Bytes held by messages waiting in HEAP's mailbox."))

(defun heap-check-stores (heap)
  "How HEAP's store barrier reacts to ownership violations: NIL, :RECORD
or :ERROR.  Setfable."
  (declare (type heap heap))
  (if (heap-released-p heap)
      nil
      (ecase (%heap-stat (heap-sap heap) +stat-store-check+)
        (0 nil) (1 :record) (2 :error))))

(defun heap-strict-p (heap)
  "True if HEAP forbids its process from mutating global objects. Setfable."
  (declare (type heap heap))
  (and (not (heap-released-p heap))
       (/= 0 (%heap-stat (heap-sap heap) +stat-strict+))))

(defun (setf heap-check-stores) (mode heap)
  (declare (type (member nil :record :error) mode))
  (%heap-set-flags (heap-sap-or-lose heap) (%heap-flags mode (heap-strict-p heap)))
  mode)

(defun (setf heap-strict-p) (strict heap)
  (%heap-set-flags (heap-sap-or-lose heap)
                   (%heap-flags (heap-check-stores heap) strict))
  strict)

(defun (setf heap-fullsweep-after) (count heap)
  (declare (type (integer 0) count))
  (%heap-set-fullsweep-after (heap-sap-or-lose heap) count)
  count)

(defun heap-gc (&optional (heap (current-heap)) full)
  "Collect HEAP, which must be installed on the current thread.  Only
HEAP's objects are traced and swept; other threads keep running.  A
minor collection reclaims the young generation (everything allocated
since the previous collection) and promotes its survivors; with FULL,
or when the heap's policy calls for it, the old generation is collected
too."
  (unless heap
    (error 'no-current-heap-error :operation 'heap-gc))
  (let ((rc (sb-vm::process-heap-collect (heap-sap-or-lose heap) full)))
    (case rc
      (0 t)
      (-1 (error 'heap-not-current-error :heap heap))
      (-2 (error 'dead-heap-error :heap heap))
      (t (error "unexpected return code ~D collecting heap" rc)))))

;;; --- Copying objects between heaps ---

(defun local-gc-concurrency-peak ()
  "Largest number of local collections that have run at the same time on
different threads since startup."
  (%heap-stat (sb-sys:int-sap 0) +stat-concurrency-peak+))

(defun %untransferable (object)
  (error 'untransferable-object :object object))

(defun copy-for-transfer (object)
  "Return a copy of OBJECT in which every sub-object owned by a process
heap has been replaced by a copy allocated in the current heap (or the
global heap if none is installed).  Global objects are shared, cycles
and shared structure are preserved.  Signals UNTRANSFERABLE-OBJECT for
closures, weak pointers, weak hash tables, foreign pointers, streams,
threads, fibers, heaps and synchronization objects."
  (let ((table (make-hash-table :test 'eq)))
    (unwind-protect (%copy-object object table)
      ;; Drop the (destination-heap) table's pointers back into the
      ;; source heap before it becomes garbage.
      (clrhash table))))

(defun %copy-object (x table)
  (cond ((or (sb-int:fixnump x) (zerop (sb-vm::object-owner x))) x)
        ((gethash x table))
        (t (%copy-process-object x table))))

(defun %copy-process-object (x table)
  (typecase x
    (cons (%copy-list-structure x table))
    (simple-vector
     (let ((new (make-array (length x))))
       (setf (gethash x table) new)
       (dotimes (i (length x) new)
         (setf (svref new i) (%copy-object (svref x i) table)))))
    ((simple-array * (*))
     (let ((new (copy-seq x)))
       (setf (gethash x table) new)
       new))
    (array (%copy-array-header x table))
    (symbol
     (let ((new (make-symbol (copy-seq (symbol-name x)))))
       (setf (gethash x table) new)
       new))
    (number (%copy-number x table))
    ((or function weak-pointer sb-sys:system-area-pointer stream
         sb-thread:mutex sb-thread:thread sb-thread::waitqueue
         sb-thread:semaphore heap)
     (%untransferable x))
    (hash-table (%copy-hash-table x table))
    (sb-kernel:instance (%copy-instance x table))
    (t (%untransferable x))))

(defun %copy-list-structure (x table)
  (let* ((head (cons nil nil))
         (tail head)
         (current x))
    (setf (gethash x table) head)
    (loop
      (setf (car tail) (%copy-object (car current) table))
      (let ((next (cdr current)))
        (cond ((and (consp next)
                    (not (zerop (sb-vm::object-owner next)))
                    (not (gethash next table)))
               (let ((new (cons nil nil)))
                 (setf (gethash next table) new
                       (cdr tail) new
                       tail new
                       current next)))
              (t
               (setf (cdr tail) (%copy-object next table))
               (return)))))
    head))

(defun %copy-array-header (x table)
  (let ((dims (array-dimensions x))
        (element-type (array-element-type x))
        (fill-pointer (and (array-has-fill-pointer-p x) (fill-pointer x)))
        (adjustable (adjustable-array-p x)))
    (multiple-value-bind (displaced-to offset) (array-displacement x)
      (if displaced-to
          (let ((new (make-array dims :element-type element-type
                                      :displaced-to (%copy-object displaced-to table)
                                      :displaced-index-offset offset
                                      :adjustable adjustable
                                      :fill-pointer fill-pointer)))
            (setf (gethash x table) new)
            new)
          (let ((new (make-array dims :element-type element-type
                                      :adjustable adjustable
                                      :fill-pointer fill-pointer)))
            (setf (gethash x table) new)
            (dotimes (i (array-total-size x) new)
              (setf (row-major-aref new i)
                    (%copy-object (row-major-aref x i) table))))))))

(defun %copy-number (x table)
  (let ((new
          (etypecase x
            (bignum
             (let* ((len (sb-bignum:%bignum-length x))
                    (new (sb-bignum:%allocate-bignum len)))
               (dotimes (i len new)
                 (sb-bignum:%bignum-set new i (sb-bignum:%bignum-ref x i)))))
            (double-float
             (sb-kernel:%make-double-float (sb-kernel:double-float-bits x)))
            (ratio
             (sb-kernel:%make-ratio (%copy-object (numerator x) table)
                                    (%copy-object (denominator x) table)))
            ((complex single-float) (complex (realpart x) (imagpart x)))
            ((complex double-float) (complex (realpart x) (imagpart x)))
            (complex
             (sb-kernel:%make-complex (%copy-object (realpart x) table)
                                      (%copy-object (imagpart x) table))))))
    (setf (gethash x table) new)
    new))

(defun %copy-hash-table (x table)
  (when (hash-table-weakness x)
    (%untransferable x))
  (let ((new (make-hash-table :test (hash-table-test x)
                              :size (max 7 (hash-table-count x))
                              :rehash-size (hash-table-rehash-size x)
                              :rehash-threshold (hash-table-rehash-threshold x)
                              :synchronized (hash-table-synchronized-p x))))
    (setf (gethash x table) new)
    (maphash (lambda (k v)
               (setf (gethash (%copy-object k table) new)
                     (%copy-object v table)))
             x)
    new))

(defun %copy-instance (x table)
  (let* ((layout (sb-kernel:%instance-layout x))
         (len (sb-kernel:%instance-length x))
         (new (if (logtest (sb-kernel:layout-flags layout)
                           sb-vm::+strictly-boxed-flag+)
                  (sb-kernel:%make-instance len)
                  (sb-kernel:%make-instance/mixed len))))
    (sb-kernel:%set-instance-layout new layout)
    (setf (gethash x table) new)
    (sb-kernel::do-layout-bitmap (i taggedp layout len)
      (if taggedp
          (sb-kernel:%instance-set new i (%copy-object (sb-kernel:%instance-ref x i) table))
          (sb-kernel:%raw-instance-set/word
           new i (sb-kernel:%raw-instance-ref/word x i))))
    new))

(defun globalize (object)
  "Return OBJECT if it is global, else a copy of it in the global heap."
  (if (or (sb-int:fixnump object) (zerop (sb-vm::object-owner object)))
      object
      (without-heap (copy-for-transfer object))))

;;; --- Shared binaries ---

(defun make-shared-binary (contents-or-length &key (element-type '(unsigned-byte 8)))
  "Return a fresh simple vector of ELEMENT-TYPE in the global heap, of
the given length or holding the given CONTENTS.  Every process heap may
refer to a global object, so a shared binary travels in messages
without being copied and is reclaimed by a global collection once no
heap refers to it any more; treat it as immutable once it has been
sent.  Objects allocated while a heap is installed are copied when
sent, which is what makes a large payload worth sharing."
  (without-heap
    (etypecase contents-or-length
      (integer (make-array contents-or-length :element-type element-type))
      (sequence (make-array (length contents-or-length)
                            :element-type element-type
                            :initial-contents contents-or-length)))))

(defun shared-binary-p (object)
  "True if OBJECT is a specialized simple vector in the global heap: one
that every heap may share."
  (and (typep object '(and (simple-array * (*)) (not simple-vector)))
       (zerop (sb-vm::object-owner object))))

;;; --- Mailboxes ---

(defun %send-to-heap (heap object)
  "Copy OBJECT into a fresh message fragment and enqueue it on HEAP."
  (declare (type heap heap))
  (heap-sap-or-lose heap)
  (let ((fragment (without-heap (%heap-create +heap-kind-fragment+ 0 0 0)))
        (root nil)
        (failure nil)
        (failure-message nil))
    (when (zerop (sb-sys:sap-int fragment))
      (error "failed to allocate a message fragment"))
    (unwind-protect
         (progn
           ;; Build the copy with the fragment installed. Conditions
           ;; signaled inside it live in the fragment, so translate them
           ;; before the fragment is discarded.
           (let ((prev (current-heap-address)))
             (%switch-heap (sb-sys:sap-int fragment))
             (unwind-protect
                  (handler-case (setf root (copy-for-transfer object))
                    (untransferable-object (c)
                      (setf failure (untransferable-object-object c)))
                    (error (c)
                      (setf failure-message (without-heap (princ-to-string c)))))
               (%switch-heap prev)))
           (cond (failure (%untransferable failure))
                 (failure-message
                  (error "copying a message for ~S failed: ~A" heap failure-message)))
           (let ((rc (%heap-seal fragment)))
             (unless (zerop rc)
               (error "sealing a message fragment failed: ~D" rc)))
           (setf (sb-sys:sap-ref-lispobj fragment (%fragment-root-offset)) root)
           (let ((rc (%heap-send-id (heap-id heap) (heap-epoch heap)
                                    fragment (%sender-id))))
             (case rc
               (0 (setf fragment nil))
               (-2 (error 'dead-heap-error :heap heap))
               (t (error "unexpected return code ~D sending to ~S" rc heap)))))
      (when fragment
        (%heap-release fragment)))
    (values)))

(define-alien-routine ("process_heap_root_offset" %fragment-root-offset) int)

(defun %sender-id ()
  (let ((addr (current-heap-address)))
    (if (zerop addr) 0 (%heap-stat (sb-sys:int-sap addr) +stat-id+))))

(defun receive-message (&key (heap (current-heap)))
  "Take the oldest message from HEAP's mailbox.  HEAP must be installed
on the current thread.  Returns the message, T, and the sending heap (or
NIL); or NIL, NIL, NIL if the mailbox is empty.  The message's storage
is adopted into HEAP without copying."
  (unless heap
    (error 'no-current-heap-error :operation 'receive-message))
  (let ((sap (heap-sap-or-lose heap)))
    (sb-alien:with-alien ((found sb-alien:int)
                          (sender (sb-alien:unsigned 32)))
      (let ((root (sb-alien:alien-funcall
                   (sb-alien:extern-alien
                    "process_heap_receive"
                    (function sb-alien:unsigned-long sb-sys:system-area-pointer
                              (* sb-alien:int) (* (sb-alien:unsigned 32))))
                   sap (sb-alien:addr found) (sb-alien:addr sender))))
        (case found
          (1 (values (sb-kernel:%make-lisp-obj root) t (%heap-from-id sender)))
          (0 (values nil nil nil))
          (-1 (error 'heap-not-current-error :heap heap))
          (t (error "unexpected return code ~D receiving from ~S" found heap)))))))

;;; --- Verification ---

(defun %collect-violations ()
  (let ((n (%heap-violation-count))
        (result '()))
    (sb-alien:with-alien ((out (sb-alien:array sb-alien:unsigned-long 3)))
      (dotimes (i n)
        (sb-alien:alien-funcall
         (sb-alien:extern-alien "process_heap_get_violation"
                                (function sb-alien:int sb-alien:int (* sb-alien:unsigned-long)))
         i (sb-alien:cast out (* sb-alien:unsigned-long)))
        (let ((source (sb-alien:deref out 0)))
          (push (list (if (zerop source) nil (sb-kernel:%make-lisp-obj source))
                      (sb-alien:deref out 1)
                      (sb-alien:deref out 2))
                result))))
    (nreverse result)))

(defun verify-heap (&optional (heap (current-heap)))
  "Check every pointer held by HEAP's objects against the ownership
rules.  Returns a list of (SOURCE-OBJECT SLOT-ADDRESS TARGET-ADDRESS)
for each pointer into another process heap or a freed page.  HEAP must
be installed on the current thread."
  (unless heap
    (error 'no-current-heap-error :operation 'verify-heap))
  (%heap-reset-violations)
  (let ((rc (%heap-verify (heap-sap-or-lose heap))))
    (case rc
      (0 (without-heap (%collect-violations)))
      (-1 (error 'heap-not-current-error :heap heap))
      (-2 (error 'dead-heap-error :heap heap))
      (t (error "unexpected return code ~D verifying heap" rc)))))

(defun heap-reference-checking ()
  "True if garbage collections record ownership violations."
  (/= 0 (sb-alien:extern-alien "process_heap_check_refs" sb-alien:int)))

(defun (setf heap-reference-checking) (enable)
  (setf (sb-alien:extern-alien "process_heap_check_refs" sb-alien:int)
        (if enable 1 0))
  enable)

(defun heap-violations ()
  "The ownership violations recorded by collections since the last
reset, as a list of (SOURCE-OBJECT SLOT-ADDRESS TARGET-ADDRESS)."
  (without-heap (%collect-violations)))

(defun reset-heap-violations ()
  (%heap-reset-violations)
  (values))

(defun verify-all-heaps ()
  "Run a full global collection with reference checking enabled and
return the ownership violations it found: pointers from the global heap
into a process heap, and pointers between different process heaps."
  (let ((was (heap-reference-checking)))
    (unwind-protect
         (progn
           (setf (heap-reference-checking) t)
           (%heap-reset-violations)
           (sb-ext:gc :full t)
           (heap-violations))
      (setf (heap-reference-checking) was))))

) ; end PROGN
