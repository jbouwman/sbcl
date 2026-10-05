;;;; -*-  Lisp -*-
;;;;
;;;; Per-local heaps: Lisp API, message copying, and mailboxes.
;;;;
;;;; A heap is a GC ownership domain inside dynamic space.  Objects
;;;; allocated while a heap is installed on the current thread belong
;;;; to it; they may refer to global objects and to each other, but
;;;; nothing global and no other heap may refer to them.  Under that
;;;; invariant a heap can be collected on its own (HEAP-GC) and freed
;;;; wholesale (RELEASE-HEAP), and objects are moved between heaps by
;;;; copying (SEND-MESSAGE / RECEIVE-MESSAGE).

(in-package :sb-fiber)

#+sb-local-heaps
(progn

;;; --- Runtime bindings ---

(define-alien-routine ("local_heap_create" %heap-create) system-area-pointer
  (kind int)
  (gc-threshold unsigned-long)
  (hard-limit unsigned-long)
  (flags int))

(define-alien-routine ("local_heap_set_flags" %heap-set-flags) void
  (heap system-area-pointer)
  (flags int))

(define-alien-routine ("local_heap_set_fullsweep_after" %heap-set-fullsweep-after) void
  (heap system-area-pointer)
  (count unsigned-long))

(define-alien-routine ("local_heap_release" %heap-release) int
  (heap system-area-pointer))

(define-alien-routine ("local_heap_stat" %heap-stat) unsigned-long
  (heap system-area-pointer)
  (which int))

;;; Entry points for threads other than the owner take the heap's id and
;;; epoch and look it up under the runtime's table lock: a released
;;; heap's descriptor is gone and its id may name a newer heap.
(define-alien-routine ("local_heap_stat_id" %heap-stat-id) unsigned-long
  (id (unsigned 32))
  (epoch (unsigned 32))
  (which int))

(define-alien-routine ("local_heap_release_id" %heap-release-id) int
  (id (unsigned 32))
  (epoch (unsigned 32)))

(define-alien-routine ("local_heap_send_id" %heap-send-id) int
  (dest-id (unsigned 32))
  (dest-epoch (unsigned 32))
  (fragment system-area-pointer)
  (sender (unsigned 32)))

(define-alien-routine ("local_heap_verify" %heap-verify) int
  (heap system-area-pointer))

(define-alien-routine ("local_heap_violation_count" %heap-violation-count) int)
(define-alien-routine ("local_heap_violation_capacity" %heap-violation-capacity) int)

;;; How many violations TAKE-HEAP-VIOLATIONS makes room for.  The
;;; runtime's own record may be larger or smaller; it copies as many as
;;; fit and reports the number noted either way, so the two need not
;;; agree for the count to be exact.
(defconstant +violation-detail-entries+ 64)

;;; Words per entry in the runtime's record: LOCAL_HEAP_VIOLATION_WORDS.
(defconstant +violation-words+ 6)
(define-alien-routine ("local_heap_reset_violations" %heap-reset-violations) void)

(define-alien-routine ("local_heap_seal" %heap-seal) int
  (fragment system-area-pointer))

(define-alien-routine ("local_heap_send" %heap-send) int
  (dest system-area-pointer)
  (fragment system-area-pointer)
  (sender (unsigned 32)))

(defconstant +heap-kind-process+ 1)
(defconstant +heap-kind-fragment+ 2)
(defconstant +heap-kind-build+ 3)

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
(defconstant +stat-installed-on+ 12)
(defconstant +stat-bytes-claimed+ 24)
(defconstant +stat-alloc-trap+ 25)
(defconstant +stat-kind+ 26)

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
  (:documentation "Signaled by an operation on a heap that has been released.")
  (:report (lambda (c stream)
             (format stream "heap ~S has been released" (heap-error-heap c)))))

(define-condition heap-in-use-error (heap-error) ()
  (:documentation "Signaled by WITH-HEAP when the heap is installed on another thread.")
  (:report (lambda (c stream)
             (format stream "heap ~S is installed on another thread"
                     (heap-error-heap c)))))

(define-condition heap-not-current-error (heap-error) ()
  (:documentation "Signaled by an operation that needs its heap installed on the
current thread, such as HEAP-GC and RECEIVE-MESSAGE, when another heap or
none is.")
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
   "An object owned by a local heap was about to be stored where only
global objects may live.")
  (:report (lambda (c stream)
             (format stream "~S is owned by a local heap and cannot be ~
                             referenced from the global heap"
                     (cross-heap-reference-object c)))))

;;; --- The heap object ---

(defstruct (heap (:constructor %make-heap)
                 (:print-object %print-heap))
  "A local heap: the handle on a set of dynamic-space blocks that one
process owns. See MAKE-HEAP."
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
;;; installed, so its storage never lands in a local heap.
(define-load-time-global *heaps* (make-hash-table :synchronized t))

(declaim (inline current-heap-address heap-active-p))
(defun current-heap-address ()
  (sb-vm::current-local-heap-address))
(defun heap-active-p ()
  (/= 0 (current-heap-address)))

;;; ADDRESS is a raw heap address (0 for the global heap).  Must not
;;; allocate: it runs in cleanups while an exhausted heap may be installed.
(defun %switch-heap (address &optional heap)
  (declare (type sb-vm:word address))
  (let ((rc (sb-vm::%switch-local-heap address)))
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
  "Execute BODY with the global heap installed on the current heap's
behalf, so that everything it allocates is shared rather than owned by
the current local heap, and so that under a strict heap its stores into
global objects are accepted. A store that would make a global object
refer into a local heap is refused as elsewhere. Restores the previous
heap on exit."
  (let ((prev (gensym "PREV")) (check (gensym "CHECK")))
    `(let ((,prev (current-heap-address)))
       (if (zerop ,prev)
           (progn ,@body)
           ;; The checking state on entry is restored on exit, as
           ;; SB-KERNEL::CALL-WITH-GLOBAL-HEAP restores it.
           (let ((,check (sb-sys:sap-int (sb-vm::current-thread-offset-sap
                                          sb-vm::thread-local-heap-check-slot))))
             (sb-vm::%install-global-heap)
             (unwind-protect (progn ,@body)
               (%switch-heap ,prev)
               (sb-vm::%store-check-resume ,check)))))))

(defun call-with-heap (heap thunk)
  (declare (function thunk) (dynamic-extent thunk))
  (let ((prev (current-heap-address))
        (address (sb-sys:sap-int (heap-sap-or-lose heap)))
        ;; Restored with PREV: reinstalling PREV would set the checking
        ;; state from its mode and end a suspension around this form.
        (check (sb-sys:sap-int (sb-vm::current-thread-offset-sap
                                sb-vm::thread-local-heap-check-slot))))
    (%switch-heap address heap)
    (unwind-protect (funcall thunk)
      (%switch-heap prev)
      (sb-vm::%store-check-resume check))))

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
                       (kind :process)
                       (gc-threshold *default-heap-gc-threshold*)
                       (hard-limit 0)
                       (fullsweep-after 16)
                       (check-stores :error)
                       strict)
  "Create a local heap.  Objects allocated while it is installed (see
WITH-HEAP and MAKE-FIBER's :HEAP argument) belong to it.  GC-THRESHOLD
is the number of bytes the heap may allocate before its young
generation is collected automatically; 0 disables automatic
collection.  FULLSWEEP-AFTER is the number of minor collections after
which the next collection is a full one; 0 leaves full collections to
HEAP-GC :FULL and to the old generation doubling in size.  HARD-LIMIT
bounds the heap's total size; 0 means unbounded.

CHECK-STORES controls the store barrier while the heap is installed:
:ERROR (the default) signals HEAP-STORE-ERROR when a pointer
store would make a global object or another heap refer into this heap,
:RECORD only records such stores (see HEAP-VIOLATIONS), NIL does not
check.  STRICT additionally treats any store into a global object as a
violation, so that a process can only mutate its own data.  Neither mode
checks a store whose value the compiler knows to be global: a literal
constant, or a value whose type admits only immediates, NIL, T and
symbols of the initial core.

KIND :BUILD makes a heap for building a core fragment.  While installed
it takes every allocation of its thread, including code, system
allocations and WITH-GLOBAL-HEAP bodies; it claims whole pages and is
never collected, so GC-THRESHOLD and FULLSWEEP-AFTER do not apply."
  (declare (type (integer 0) gc-threshold hard-limit fullsweep-after)
           (type (member :process :build) kind)
           (type (member nil :record :error) check-stores))
  (without-heap
    (let ((sap (%heap-create (ecase kind
                               (:process +heap-kind-process+)
                               (:build +heap-kind-build+))
                             gc-threshold hard-limit
                             (%heap-flags check-stores strict))))
      (when (zerop (sb-sys:sap-int sap))
        (error "failed to allocate a local heap"))
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
HEAP must not be installed on another thread.  A build heap cannot be
released: global tables refer into it once it has been used."
  (declare (type heap heap))
  (unless (heap-released-p heap)
    (when (eq (heap-kind heap) :build)
      (error "~S is a build heap, which cannot be released" heap))
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

(defun heap-kind (heap)
  "The kind HEAP was made with: :PROCESS, :BUILD or :FRAGMENT."
  (declare (type heap heap))
  (let ((kind (%heap-stat (heap-sap-or-lose heap) +stat-kind+)))
    (cond ((= kind +heap-kind-process+) :process)
          ((= kind +heap-kind-build+) :build)
          ((= kind +heap-kind-fragment+) :fragment)
          (t (error "heap ~S has unknown kind ~D" heap kind)))))

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
  (def heap-bytes-claimed +stat-bytes-claimed+
    "Bytes HEAP has claimed from dynamic space since it was made, block
granular.")
  (def heap-hard-limit +stat-hard-limit+
    "The size past which HEAP refuses to grow, or 0 for none.  Setfable.")
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

;;; Suspend the store barrier around a bookkeeping store made on a
;;; fiber's behalf (see fiber.lisp and fiber-ffi.lisp). The store is
;;; still made, and is still unsafe if what it stores outlives the heap
;;; that owns it.
(defmacro without-store-checking (&body body)
  (let ((saved (gensym "SAVED")))
    `(let ((,saved (sb-vm::%store-check-suspend)))
       (unwind-protect (progn ,@body)
         (sb-vm::%store-check-resume ,saved)))))

(defun (setf heap-fullsweep-after) (count heap)
  (declare (type (integer 0) count))
  (%heap-set-fullsweep-after (heap-sap-or-lose heap) count)
  count)

;;; --- Allocation trap ---

(define-alien-routine ("local_heap_arm_alloc_trap_address" %heap-arm-alloc-trap)
    unsigned-long
  (heap unsigned-long)
  (bytes unsigned-long))

(define-alien-routine ("local_heap_disarm_alloc_trap_address" %heap-disarm-alloc-trap)
    void
  (heap unsigned-long))

(define-alien-routine ("local_heap_set_hard_limit_address" %heap-set-hard-limit)
    void
  (heap unsigned-long)
  (limit unsigned-long))

(defun %owned-heap-address (heap)
  (let* ((sap (heap-sap-or-lose heap))
         (on (%heap-stat sap +stat-installed-on+)))
    (unless (or (zerop on)
                (= on (sb-sys:sap-int (sb-thread::current-thread-sap))))
      (error 'heap-in-use-error :heap heap))
    (sb-sys:sap-int sap)))

(defun arm-heap-allocation-trap (heap bytes)
  "Arm HEAP's allocation trap to fire once HEAP-BYTES-CLAIMED has grown by
more than BYTES from its present value."
  (declare (type heap heap)
           (type (unsigned-byte 62) bytes))
  (%heap-arm-alloc-trap (%owned-heap-address heap) bytes))

(defun disarm-heap-allocation-trap (heap)
  "Disarm HEAP's allocation trap, including one that has fired and not yet
been delivered."
  (declare (type heap heap))
  (unless (heap-released-p heap)
    (%heap-disarm-alloc-trap (%owned-heap-address heap)))
  nil)

(defun heap-allocation-trap-threshold (heap)
  "The HEAP-BYTES-CLAIMED value past which HEAP's armed allocation trap
fires."
  (declare (type heap heap))
  (let ((trap (if (heap-released-p heap)
                  0
                  (%heap-stat-id (heap-id heap) (heap-epoch heap)
                                 +stat-alloc-trap+))))
    (if (zerop trap) nil trap)))

(defun heap-allocation-trap-heap (condition)
  "The heap whose allocation trap fired, or NIL if it has been released."
  (let ((heap (%heap-from-id (sb-kernel::heap-allocation-trap-heap-id condition))))
    (and heap
         (= (heap-epoch heap)
            (sb-kernel::heap-allocation-trap-heap-epoch condition))
         heap)))

(defun (setf heap-hard-limit) (limit heap)
  "Set the size past which HEAP refuses to grow; 0 removes the bound."
  (declare (type heap heap)
           (type (unsigned-byte 62) limit))
  (%heap-set-hard-limit (%owned-heap-address heap) limit)
  limit)

(defun heap-gc (&optional (heap (current-heap)) full)
  "Collect HEAP, which must be installed on the current thread.  Only
HEAP's objects are traced and swept; other threads keep running."
  (unless heap
    (error 'no-current-heap-error :operation 'heap-gc))
  (let ((rc (sb-vm::local-heap-collect (heap-sap-or-lose heap) full)))
    (case rc
      (0 t)
      (-1 (error 'heap-not-current-error :heap heap))
      (-2 (error 'dead-heap-error :heap heap))
      (-3 (error "~S is a build heap, which is never collected" heap))
      (t (error "unexpected return code ~D collecting heap" rc)))))

;;; --- Building core fragments ---

(defun make-fragment-recorder (heap)
  "Make the recorder of a core fragment built in HEAP, a build heap.  Loading
a module with HEAP installed and the recorder bound (WITH-FRAGMENT-RECORDER)
notes as records the effects its definitions have on global objects outside
the fragment, and accounts for every store that makes a global object refer
into HEAP: to a record, to a cache, or as unaccounted.  The recorder is
allocated in HEAP."
  (declare (type heap heap))
  (unless (eq (heap-kind heap) :build)
    (error "~S is not a build heap" heap))
  (with-heap (heap) (sb-vm::make-fragment-recorder)))

(defmacro with-fragment-recorder ((recorder) &body body)
  "Execute BODY with RECORDER noting records and accounting for escapes, for
whatever BODY does while RECORDER's heap is installed."
  `(let ((sb-kernel::*fragment-recorder* ,recorder))
     ,@body))

(defmacro with-fragment-record ((kind target &rest args) &body body)
  "Execute BODY as a registration's effect on TARGET, a global object.
While a fragment is being built and TARGET is not the fragment's, the
effect is noted as a record of KIND with ARGS, and the stores BODY makes
into global objects are accounted to it.  A KIND of :CALL names, as the
first of ARGS, a function that activating the fragment calls with the
rest; :PUT records storing the second of ARGS under the first in TARGET,
a hash table.  Outside a build, BODY runs as it is."
  `(sb-kernel::with-fragment-record (,kind ,target ,@args) ,@body))

(defun fragment-records (recorder)
  "The records RECORDER noted, oldest first."
  (reverse (sb-vm::fragment-recorder-records recorder)))

(defun fragment-linkage-sites (recorder)
  "The linkage cell references the loader patched into the code of the
fragment RECORDER was recording: a hash table from the address of a code
object to its sites, each (offset kind index), newest first.  Empty on a
target without linkage space."
  (sb-vm::fragment-recorder-linkage-sites recorder))

(defun fragment-record-kind (record) (sb-vm::fragment-record-kind record))
(defun fragment-record-target (record) (sb-vm::fragment-record-target record))
(defun fragment-record-args (record) (sb-vm::fragment-record-args record))
(defun fragment-record-escapes (record)
  "The number of escapes RECORD accounts for."
  (sb-vm::fragment-record-escapes record))
(defun fragment-record-replay-values (record)
  "What RECORD's target held when its fragment was sealed, which replaying
RECORD installs, for the kinds whose effect is a value: :SET-FUNCTION,
:SET-VALUE and :SET-INFO."
  (sb-vm::fragment-record-replay-values record))

(defun fragment-unaccounted-escapes (recorder)
  "The escapes no record or cache accounts for, oldest first, as lists
(OBJECT VALUE STORE-PC): a global OBJECT made to refer to VALUE, of the
fragment, by the code STORE-SITE names for STORE-PC."
  (reverse (sb-vm::fragment-recorder-unaccounted recorder)))

(defun fragment-cache-escapes (recorder)
  "The number of escapes RECORDER accounted to caches."
  (sb-vm::fragment-recorder-cache-escapes recorder))

(defun seal-fragment (recorder)
  "Finish the fragment RECORDER was recording.  Each record notes the value
it replays, the build heap is sealed so that it is never installed again,
and the fragment's objects are found: those of the heap RECORDER reaches.
What else the load left in the heap is not part of the fragment.  Returns
the number of the fragment's objects.  The heap must not be installed;
sealing a fragment with unaccounted escapes is a continuable error."
  (sb-vm::seal-fragment recorder))

(defun map-fragment-members (function recorder)
  "Call FUNCTION on each object of the fragment RECORDER was recording, in
address order.  RECORDER's fragment must be sealed."
  (sb-vm::map-fragment-members (coerce function (quote function)) recorder))

;;; --- Copying objects between heaps ---

(defun local-gc-concurrency-peak ()
  "Largest number of local collections that have run at the same time on
different threads since startup."
  (%heap-stat (sb-sys:int-sap 0) +stat-concurrency-peak+))

(defun %untransferable (object)
  ;; The condition refers to OBJECT, so it is built in the heap that owns
  ;; OBJECT: built with the global heap installed, as GLOBALIZE installs
  ;; it, it would be a global object referring into a heap, the escape
  ;; the barrier refuses. When that heap is installed on another thread
  ;; the condition names the object's type instead.
  (flet ((refuse () (error 'untransferable-object :object object)))
    (declare (dynamic-extent #'refuse))
    (if (or (sb-int:fixnump object) (zerop (sb-vm::object-owner object)))
        (refuse)
        (progn
          (sb-kernel::call-with-heap-of object #'refuse)
          (error 'untransferable-object :object (type-of object))))))

(defun copy-for-transfer (object)
  "Return a copy of OBJECT in which every sub-object owned by a process
heap has been replaced by a copy allocated in the current heap (or the
global heap if none is installed)."
  (let ((table (make-hash-table :test 'eql)))
    (unwind-protect (%copy-object object table)
      (clrhash table))))

;;; The table of copies made so far is keyed by the source object's
;;; address.  It lives where the copies do, often the global heap, and
;;; keyed by the objects themselves it would refer into the source heap:
;;; a vector it outgrew keeps its entries, and a conservative root that
;;; retains one after the source heap is released leads a global
;;; collection into freed or reused memory.  Local objects do not move,
;;; and only local objects are entered.
(declaim (inline %seen (setf %seen)))
(defun %seen (x table)
  (gethash (sb-kernel:get-lisp-obj-address x) table))
(defun (setf %seen) (new x table)
  (setf (gethash (sb-kernel:get-lisp-obj-address x) table) new))

(defun %copy-object (x table)
  (cond ((or (sb-int:fixnump x) (zerop (sb-vm::object-owner x))) x)
        ((%seen x table))
        (t (%copy-process-object x table))))

(defun %copy-process-object (x table)
  (typecase x
    (cons (%copy-list-structure x table))
    (simple-vector
     (let ((new (make-array (length x))))
       (setf (%seen x table) new)
       (dotimes (i (length x) new)
         (setf (svref new i) (%copy-object (svref x i) table)))))
    ((simple-array * (*))
     (let ((new (copy-seq x)))
       (setf (%seen x table) new)
       new))
    (array (%copy-array-header x table))
    (symbol
     (let ((new (make-symbol (copy-seq (symbol-name x)))))
       (setf (%seen x table) new)
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
    (setf (%seen x table) head)
    (loop
      (setf (car tail) (%copy-object (car current) table))
      (let ((next (cdr current)))
        (cond ((and (consp next)
                    (not (zerop (sb-vm::object-owner next)))
                    (not (%seen next table)))
               (let ((new (cons nil nil)))
                 (setf (%seen next table) new
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
            (setf (%seen x table) new)
            new)
          (let ((new (make-array dims :element-type element-type
                                      :adjustable adjustable
                                      :fill-pointer fill-pointer)))
            (setf (%seen x table) new)
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
    (setf (%seen x table) new)
    new))

(defun %copy-hash-table (x table)
  (when (hash-table-weakness x)
    (%untransferable x))
  (let ((new (make-hash-table :test (hash-table-test x)
                              :size (max 7 (hash-table-count x))
                              :rehash-size (hash-table-rehash-size x)
                              :rehash-threshold (hash-table-rehash-threshold x)
                              :synchronized (hash-table-synchronized-p x))))
    (setf (%seen x table) new)
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
    (setf (%seen x table) new)
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
the given length or holding the given CONTENTS."
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

(define-alien-routine ("local_heap_root_offset" %fragment-root-offset) int)

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
                    "local_heap_receive"
                    (function sb-alien:unsigned-long sb-sys:system-area-pointer
                              (* sb-alien:int) (* (sb-alien:unsigned 32))))
                   sap (sb-alien:addr found) (sb-alien:addr sender))))
        (case found
          (1 (values (sb-kernel:%make-lisp-obj root) t (%heap-from-id sender)))
          (0 (values nil nil nil))
          (-1 (error 'heap-not-current-error :heap heap))
          (t (error "unexpected return code ~D receiving from ~S" found heap)))))))

;;; --- Verification ---

;;; A violation as the runtime records it, decoded from the words at
;;; INDEX in the alien word array WORDS: (SOURCE-OBJECT SLOT-ADDRESS
;;; TARGET-ADDRESS ORIGIN STORE-PC KIND).  ORIGIN is :COLLECTION for a
;;; pointer a collection or verification found, :RECORDED-STORE for a store
;;; the barrier recorded and let proceed, and :SIGNALED-STORE for one it
;;; signaled HEAP-STORE-ERROR for, which refused the store unless a
;;; handler continued it.  STORE-PC is the return address into the code
;;; that made the store, or NIL for a collection; STORE-SITE names it.
;;; KIND is the store's HEAP-STORE-ERROR-KIND, or NIL for a collection.
(declaim (inline %decode-violation))
(defun %decode-violation (words index)
  (flet ((word (n) (sb-alien:deref words (+ (* +violation-words+ index) n))))
    (let ((source (word 0))
          (pc (word 4)))
      (list (if (zerop source) nil (sb-kernel:%make-lisp-obj source))
            (word 1)
            (word 2)
            (case (word 3)
              (0 :collection)
              (1 :recorded-store)
              (2 :signaled-store)
              (t (word 3)))
            (if (zerop pc) nil pc)
            (case (word 5)
              (0 nil)
              (1 :escape)
              (2 :cross-heap)
              (3 :global)
              (t (word 5)))))))

(defun %collect-violations ()
  (let ((n (%heap-violation-count))
        (result '()))
    ;; The dimension is +VIOLATION-WORDS+; it has to be a literal, since
    ;; the whole file is read as one form before any of it is evaluated.
    (sb-alien:with-alien ((out (sb-alien:array sb-alien:unsigned-long 6)))
      (dotimes (i n)
        (sb-alien:alien-funcall
         (sb-alien:extern-alien "local_heap_get_violation"
                                (function sb-alien:int sb-alien:int (* sb-alien:unsigned-long)))
         i (sb-alien:cast out (* sb-alien:unsigned-long)))
        (push (%decode-violation out 0) result)))
    (nreverse result)))

(defun store-site (pc)
  "The name of the function whose code contains PC, the STORE-PC of a
store violation, or NIL when no code object holds PC.  The runtime keeps
the address rather than the code object, so the code can have been
collected by the time this is asked; the name is that of whatever code
holds PC then."
  (let ((code (sb-di::code-header-from-pc pc)))
    (when code
      (let ((offset (- pc (sb-sys:sap-int (sb-kernel:code-instructions code)))))
        (when (<= 0 offset)
          (sb-di:debug-fun-name (sb-di::debug-fun-from-pc code offset nil)))))))

(defun verify-heap (&optional (heap (current-heap)))
  "Check every pointer held by HEAP's objects against the ownership
rules.  Returns a list of (SOURCE-OBJECT SLOT-ADDRESS TARGET-ADDRESS
ORIGIN STORE-PC KIND), as TAKE-HEAP-VIOLATIONS describes, for each pointer
into another local heap or a freed page.  HEAP must be installed on the
current thread."
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
  (/= 0 (sb-alien:extern-alien "local_heap_check_refs" sb-alien:int)))

(defun (setf heap-reference-checking) (enable)
  (setf (sb-alien:extern-alien "local_heap_check_refs" sb-alien:int)
        (if enable 1 0))
  enable)

(defun heap-violations ()
  "The ownership violations recorded by collections and the store
barrier since the last reset, as a list of (SOURCE-OBJECT SLOT-ADDRESS
TARGET-ADDRESS ORIGIN STORE-PC KIND), as TAKE-HEAP-VIOLATIONS describes."
  (without-heap (%collect-violations)))

(defun reset-heap-violations ()
  (%heap-reset-violations)
  (values))

(defun take-heap-violations ()
  "Remove and return the ownership violations recorded since the last
call: a list of (SOURCE-OBJECT SLOT-ADDRESS TARGET-ADDRESS ORIGIN
STORE-PC KIND), and as a second value the number noted, which is exact and
may exceed the length of the list -- a burst larger than the record keeps
its first entries.

ORIGIN tells a pointer that exists from a store that was stopped:
:COLLECTION for a pointer a collection or verification found,
:RECORDED-STORE for a store the barrier recorded and let proceed under
:CHECK-STORES :RECORD, and :SIGNALED-STORE for a store the barrier
signaled HEAP-STORE-ERROR for under :CHECK-STORES :ERROR, which left the
object as it was unless a handler chose CONTINUE.  For a store,
SLOT-ADDRESS is 0 and STORE-PC is the return address into the code that
made it, which STORE-SITE names; for a collection STORE-PC is NIL.

KIND is how the barrier classified a store, as HEAP-STORE-ERROR-KIND
names it: :ESCAPE for a pointer into a local heap stored into a global
object, :CROSS-HEAP for one stored into another heap's object, and
:GLOBAL for a strict heap's store into a global object of a value that
is not an escape.  It is NIL for a collection.  It is recorded at the
store because it cannot be recovered after the fact: the heap that owned
an escaped value may have been released by the time the record is read.

Clearing the record and copying it out happen in one step.  Violations
are noted from collections and from the store barrier on any thread,
without a lock, so one noted between a HEAP-VIOLATIONS call and a
RESET-HEAP-VIOLATIONS call is lost: neither call sees it and the reset
discards it.  Use this wherever violations are drained while other
threads are running."
  ;; The buffer is stack-allocated: this runs whenever a process
  ;; finishes, so it must not allocate per call.  The runtime copies at
  ;; most as many entries as it is offered room for, and the count it
  ;; returns is exact whether or not they all fit.
  ;; 384 words is +VIOLATION-DETAIL-ENTRIES+ entries of +VIOLATION-WORDS+;
  ;; the array dimension has to be a literal, since the whole file is read
  ;; as one form before any of it is evaluated.
  (sb-alien:with-alien ((buffer (sb-alien:array sb-alien:unsigned-long 384))
                        (ndetails sb-alien:int 0))
    (let ((total (sb-alien:alien-funcall
                  (sb-alien:extern-alien
                   "local_heap_take_violations"
                   (function sb-alien:int (* sb-alien:unsigned-long)
                             sb-alien:int (* sb-alien:int)))
                  (sb-alien:cast buffer (* sb-alien:unsigned-long))
                  +violation-detail-entries+
                  (sb-alien:addr ndetails)))
          (result '()))
      (without-heap
        (dotimes (i ndetails)
          (push (%decode-violation buffer i) result))
        (values (nreverse result) total)))))

(defun verify-all-heaps ()
  "Run a full global collection with reference checking enabled and
return the ownership violations it found: pointers from the global heap
into a local heap, and pointers between different local heaps."
  (let ((was (heap-reference-checking)))
    (unwind-protect
         (progn
           (setf (heap-reference-checking) t)
           (%heap-reset-violations)
           (sb-ext:gc :full t)
           (heap-violations))
      (setf (heap-reference-checking) was))))

) ; end PROGN

;;;; Activating a fragment file

(define-alien-routine ("corefrag_activate_file" %activate-fragment-file) int
  (path c-string)
  (out (* unsigned-long)))

;;; The file's header, as the writer lays it out (corefrag-writer.lisp):
;;; magic, version and section count, then (id offset length) per
;;; section.  The sections read here are printed forms.
(defconstant +fragment-magic+ #x53424652)
(defconstant +fragment-version+ 2)
(defconstant +fragment-section-imports+ 5)
(defconstant +fragment-section-records+ 6)
(defconstant +fragment-section-linkage+ 8)

(define-alien-routine ("corefrag_patch_word" %patch-fragment-word) void
  (address unsigned-long)
  (value unsigned-long))

(defun read-fragment-word (stream)
  (let ((word 0))
    (dotimes (i sb-vm:n-word-bytes word)
      (setf (ldb (byte 8 (* 8 i)) word) (read-byte stream)))))

;;; Section ID of the fragment file at PATH, read back as the form the
;;; writer printed, or NIL when the file has no such section.
(defun read-fragment-section (path id)
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (unless (= (read-fragment-word stream) +fragment-magic+)
      (error "~A is not a fragment file" path))
    (let ((version (read-fragment-word stream)))
      (unless (= version +fragment-version+)
        (error "~A is a version ~D fragment file; this runtime reads version ~D"
               path version +fragment-version+)))
    (dotimes (i (read-fragment-word stream))
      (let ((section (read-fragment-word stream))
            (offset (read-fragment-word stream))
            (length (read-fragment-word stream)))
        (when (= section id)
          (file-position stream offset)
          (let ((octets (make-array length :element-type '(unsigned-byte 8))))
            (read-sequence octets stream)
            (return
              (with-standard-io-syntax
                (let ((*package* (find-package "KEYWORD")) (*read-eval* nil))
                  (read-from-string
                   (sb-ext:octets-to-string octets :external-format :utf-8)))))))))))

(defvar *fragment-namer* nil
  "A function of one argument, or NIL: called by the fragment writer on an
object outside the fragment that it cannot name itself, it returns a
readable form naming the object for *FRAGMENT-RESOLVER*, or NIL.")

(defvar *fragment-resolver* nil
  "A function of one argument, or NIL: called when a fragment is activated
with a form *FRAGMENT-NAMER* returned in the builder, it returns the object
the form names in this process.")

;;; The object a reference of the writer's names: a member by its planned
;;; address, now mapped; a global symbol, string, number or character as
;;; itself; a list element by element, a dotted pair by its halves; a
;;; global package, fdefn, classoid or layout by name; a ctype by its specifier and a source location by
;;; its parts, rebuilt; an object the application named, through
;;; *FRAGMENT-RESOLVER*; an object of the core by its address, which this
;;; process shares.
(defun fragment-reference-object (reference)
  (ecase (first reference)
    (:member (sb-kernel:%make-lisp-obj (second reference)))
    (:list (mapcar #'fragment-reference-object (second reference)))
    (:cons (cons (fragment-reference-object (second reference))
                 (fragment-reference-object (third reference))))
    (:global
     (if (null (cddr reference))
         (second reference)
         (destructuring-bind (kind &rest parts) (rest reference)
           (ecase kind
             (:package (or (find-package (first parts))
                           (error "no package ~A" (first parts))))
             (:symbol (let ((package (and (second parts) (fragment-reference-object (second parts)))))
                        (if package
                            (intern (first parts) package)
                            (make-symbol (first parts)))))
             (:fdefn (sb-kernel:find-or-create-fdefn (fragment-reference-object (first parts))))
             (:classoid (sb-kernel:find-classoid (fragment-reference-object (first parts))))
             (:layout (sb-kernel:find-layout (fragment-reference-object (first parts))))
             (:value (sb-ext:symbol-global-value (fragment-reference-object (first parts))))
             (:named (if *fragment-resolver*
                         (funcall *fragment-resolver* (first parts))
                         (error "a fragment refers to ~S by a name, and no resolver is set"
                                (first parts))))
             ;; VALUES-SPECIFIER-TYPE takes a VALUES specifier too, as a function's
             ;; return type is.
             (:type (sb-kernel:values-specifier-type (fragment-reference-object (first parts))))
             (:key-info (sb-kernel::make-key-info (fragment-reference-object (first parts))
                                                 (fragment-reference-object (second parts))))
             (:source-location
              (destructuring-bind (namestring indices plist) parts
                (let ((plist (fragment-reference-object plist)))
                  (if plist
                      (sb-c::%make-full-definition-source-location namestring indices plist)
                      (sb-c::%make-basic-definition-source-location namestring indices)))))
             (:address (sb-kernel:%make-lisp-obj (first parts)))
             (:unnamed (error "a fragment record refers to an object of type ~A ~
                               that the file does not name" (first parts)))))))))

;;; Resolve the fragment's imports: the words of its objects that refer to
;;; objects outside it.  An object of the core is at the same address in
;;; this process and needs nothing; any other is resolved from the
;;; reference the writer made and stored in the word, fdefns last, since
;;; an fdefn's name may be a list of the fragment whose words are being
;;; resolved.  IMPORTS is the file's imports section: per word its
;;; address and the reference.  Returns the number of words stored.
(defun resolve-fragment-imports (imports)
  (let ((resolved 0))
    (flet ((resolve (import)
             (destructuring-bind (address reference) import
               (let ((object (fragment-reference-object reference)))
                 (sb-sys:with-pinned-objects (object)
                   (%patch-fragment-word address (sb-kernel:get-lisp-obj-address object)))
                 (incf resolved))))
           (fdefn-p (import)
             (let ((reference (second import)))
               (and (eq (first reference) :global) (eq (second reference) :fdefn))))
           (core-p (import)
             (let ((reference (second import)))
               (and (eq (first reference) :global) (eq (second reference) :address)))))
      (dolist (import imports)
        (unless (or (core-p import) (fdefn-p import)) (resolve import)))
      (dolist (import imports resolved)
        (when (fdefn-p import) (resolve import))))))

;;; Resolve the names the fragment's code calls through linkage cells and
;;; patch each reference.  The builder assigned the cells it had free;
;;; this process assigns its own by ENSURE-LINKAGE-INDEX, which also fills
;;; the cell of a name the fragment defines from the name's function
;;; slot, mapped with the fragment.  The code's callee list, which the
;;; collector follows to the names, is written again with the new
;;; indices.  LINKAGE is the file's linkage section: per code object its
;;; address, then per site the offset of the reference, the fixup kind
;;; that wrote it, and a reference to the name.
#+linkage-space
(defun relink-fragment (linkage)
  (let ((nsites 0))
    (dolist (entry linkage nsites)
      (destructuring-bind (code-address &rest sites) entry
        (let ((code (sb-kernel:%make-lisp-obj code-address))
              (callees '()))
          (sb-sys:with-pinned-objects (code)
            (dolist (site sites)
              (destructuring-bind (offset kind name) site
                (let* ((fname (fragment-reference-object name))
                       (index (sb-int:ensure-linkage-index fname)))
                  (sb-vm:fixup-code-object code offset index kind :linkage-cell)
                  (unless (sb-int:permanent-fname-p
                           (if (sb-kernel:fdefn-p fname) (sb-kernel:fdefn-name fname) fname))
                    (pushnew index callees))
                  (incf nsites)))))
          (multiple-value-bind (old abs32 imm)
              (sb-c:unpack-code-fixup-locs (sb-vm::%code-fixups code))
            (declare (ignore old))
            (setf (sb-vm::%code-fixups code)
                  (sb-c:pack-code-fixup-locs callees abs32 imm))))))))

#-linkage-space
(defun relink-fragment (linkage)
  (when linkage
    (error "the fragment references linkage cells, which this runtime has none of"))
  0)

;;; Replay the fragment's records in order, each through the definer
;;; whose effect it noted, with the objects the file refers to.  A record
;;; that carries a value installs what its target held when the fragment
;;; was sealed.  Returns the number of records replayed and an alist of
;;; the kinds not replayed with their counts: the kinds whose replay is
;;; not written yet, and those whose value the target did not hold.
(defun replay-fragment-records (records)
  (let ((replayed 0) (skipped '()))
    (dolist (record records (values replayed (nreverse skipped)))
      (destructuring-bind (kind target args values) record
        (flet ((object (reference) (fragment-reference-object reference))
               (done () (incf replayed))
               (skip ()
                 (let ((entry (assoc kind skipped)))
                   (if entry (incf (cdr entry)) (push (cons kind 1) skipped)))))
          (case kind
            (:set-function
             (if values
                 (progn (sb-int:fset (object target) (object (first values))) (done))
                 (skip)))
            (:set-value
             (if values
                 (progn (setf (sb-ext:symbol-global-value (object target)) (object (first values)))
                        (done))
                 (skip)))
            (:set-info
             (if values
                 (progn (sb-int:set-info-value (object (first args)) (object (second args))
                                               (object (first values)))
                        (done))
                 (skip)))
            (:clear-info
             (sb-int:clear-info-values (object (first args)) (object (second args)))
             (done))
            (:register-package
             (let ((package (object (first args))))
               (sb-int:with-system-mutex (sb-impl::*package-table-lock*)
                 ;; The fragment's symbols carry the package id the builder
                 ;; gave the package, so the package keeps it: its slot in
                 ;; the id table must be free here, or already its own.
                 (let ((id (sb-impl::package-id package)))
                   (when id
                     (let ((vector sb-impl::*id->package*))
                       (when (>= id (length vector))
                         (let ((new (make-array (1+ id) :initial-element nil)))
                           (replace new vector)
                           (setf sb-impl::*id->package* new vector new)))
                       (let ((holder (aref vector id)))
                         (unless (or (null holder) (eq holder package))
                           (error "package id ~D of ~A is held by ~A" id package holder)))
                       (setf (aref vector id) package))))
                 (sb-impl::package-registry-update package (object (second args)))))
             (done))
            (:intern
             (let ((package (object (first args))) (symbol (object (second args))))
               (sb-thread:with-recursive-lock (sb-impl::*package-graph-lock*)
                 (sb-impl::add-symbol (if (eq package (find-package "KEYWORD"))
                                          (sb-kernel:package-external-symbols package)
                                          (sb-kernel:package-internal-symbols package))
                                      symbol 'intern)))
             (done))
            (:export (export (object (second args)) (object (first args))) (done))
            (:unexport (unexport (object (second args)) (object (first args))) (done))
            (:import (import (object (second args)) (object (first args))) (done))
            (:shadowing-import (shadowing-import (object (second args)) (object (first args))) (done))
            (:shadow (shadow (object (second args)) (object (first args))) (done))
            (:use-package (use-package (object (second args)) (object (first args))) (done))
            (:package-nickname
             ;; A package-local nickname: (package string other-package), the
             ;; last NIL to remove the nickname.
             (sb-impl::pkgnick-update (object (first args)) (object (second args))
                                      (object (third args)))
             (done))
            (:fdefn
             ;; The fdefn is the fragment's, installed under its name again:
             ;; in the symbol's info for a (SETF symbol) name, as
             ;; FIND-OR-CREATE-FDEFN would, in the table of fancily named
             ;; fdefns otherwise.
             (let ((name (object (first args))))
               (if values
                   (let ((fdefn (object (first values))))
                     (cond #-linkage-space
                           ((symbolp name)
                            ;; Without linkage space a symbol holds its fdefn.
                            (sb-vm::cas-symbol-fdefn name 0 fdefn))
                           ((and (listp name) (listp (cdr name)) (null (cddr name))
                                 (symbolp (first name)) (symbolp (second name)))
                            (sb-int:set-info-value
                             name (sb-int:meta-info-number (sb-int:meta-info :function :definition))
                             fdefn))
                           (t
                            (let ((found (sb-impl::get-fancily-named-fdefn
                                          name (lambda (name) (declare (ignore name)) fdefn))))
                              (unless (eq found fdefn)
                                (error "~S already names ~S; the fragment carries ~S"
                                       name found fdefn))))))
                   (sb-kernel:find-or-create-fdefn name)))
             (done))
            (:forward-layout
             (sb-kernel:with-world-lock ()
               (setf (gethash (object (first args)) sb-kernel::*forward-referenced-layouts*)
                     (object (second args))))
             (done))
            (:add-subclassoid
             (sb-kernel::%add-subclassoid (object (first args)) (object (second args))
                                          (object (third args)))
             (done))
            (:add-direct-subclass
             (sb-mop:add-direct-subclass (object (first args)) (object (second args)))
             (done))
            (:add-direct-method
             (sb-mop:add-direct-method (object (first args)) (object (second args)))
             (done))
            (:add-method
             (let ((gf (object (first args))) (method (object (second args))))
               ;; The method is the fragment's, and was added to this generic
               ;; function in the builder; it is added here as new.
               (when (eq (sb-mop:method-generic-function method) gf)
                 (setf (sb-mop:method-generic-function method) nil))
               (add-method gf method))
             (done))
            (:reinitialize-generic-function
             (apply #'reinitialize-instance (object (first args)) (object (second args)))
             (done))
            (:eql-specializer
             (if values
                 (progn (setf (gethash (object (first args)) (object target)) (object (first values)))
                        (done))
                 (skip)))
            (:interned
             (let ((container (object target)))
               (dolist (reference values)
                 (let ((x (object reference)))
                   (if (hash-table-p container)
                       (setf (gethash (first x) container) (second x))
                       (sb-int:hashset-insert container x)))))
             (done))
            (:call
             (apply (object (first args)) (mapcar #'object (rest args)))
             (done))
            (:put
             (setf (gethash (object (first args)) (object target)) (object (second args)))
             (done))
            (t (skip))))))))

(defun activate-fragment-file (pathname)
  "Map the page runs of the fragment file at PATHNAME at their planned
addresses and install them in the collector as pseudo-static pages; then
resolve the names the fragment's code calls through linkage cells and
patch the references, resolve its imports, and replay its records.  This
is the prebound case: the file was written against this core and its
planned pages are free, so nothing is relocated, and an import of an
object of the core needs no resolution.  Returns the number of runs and
the bytes mapped, the number of linkage references patched, the number
of records replayed, an alist of the record kinds not replayed with their
counts, and the number of imports resolved."
  (let ((path (sb-ext:native-namestring (merge-pathnames pathname) :as-file t)))
    (multiple-value-bind (runs bytes)
        (with-alien ((out (array unsigned-long 2)))
          (let ((rc (sb-sys:without-gcing
                      (%activate-fragment-file path (cast out (* unsigned-long))))))
            (case rc
              (0 (values (deref out 0) (deref out 1)))
              (-1 (error "~A is not a fragment file" path))
              (-2 (error "~A was written for another fragment file version" path))
              (-3 (error "a run of ~A lies outside dynamic space or is not page-aligned" path))
              (-4 (error "a planned page of ~A is in use" path))
              (-5 (error "mapping a run of ~A failed" path))
              (t (error "unexpected return code ~D activating ~A" rc path)))))
      (let* ((imports (resolve-fragment-imports
                       (read-fragment-section path +fragment-section-imports+)))
             (sites (relink-fragment (read-fragment-section path +fragment-section-linkage+))))
        (multiple-value-bind (replayed skipped)
            (replay-fragment-records (read-fragment-section path +fragment-section-records+))
          (values runs bytes sites replayed skipped imports))))))
