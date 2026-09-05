;;;; -*-  Lisp -*-
;;;;
;;;; Processes: spawning, mailboxes with selective receive, links,
;;;; monitors, exit signals, the registry and timers.
;;;;
;;;; A process is a global descriptor (never allocated in a process
;;;; heap) plus a fiber that runs its function.  The descriptor's slots
;;;; hold only global objects: the message a process is waiting for, the
;;;; messages it has skipped, and the value it computes live in its own
;;;; heap and reach the outside only by copying.

(in-package :sb-process)

(defvar *default-process-heap* #+sb-process-heaps t #-sb-process-heaps nil
  "What SPAWN does about a heap when not told: T gives every process a
heap of its own, NIL makes processes share the global heap.")

(defvar *default-process-stack-size* (* 256 1024)
  "Control stack size, in bytes, of a process fiber.")

(defvar *reductions-per-slice* 2000
  "Number of message operations a process may perform before it yields
to the other processes on its carrier.")

(defvar *report-process-errors* t
  "Whether a process that exits through an unhandled error reports it on
*ERROR-OUTPUT*: NIL, T, or :BACKTRACE to include a backtrace.")

(defvar *current-process* nil
  "Bound in each process to its descriptor.")

;;; --- Conditions ---

(define-condition process-error (error)
  ((process :initarg :process :initform nil :reader process-error-process)))

(define-condition no-current-process-error (process-error)
  ((operation :initarg :operation :initform nil :reader no-current-process-error-operation))
  (:report (lambda (c stream)
             (format stream "~@[~S ~]called outside of a process"
                     (no-current-process-error-operation c)))))

(define-condition dead-process-error (process-error) ()
  (:report (lambda (c stream)
             (format stream "~S has exited" (process-error-process c)))))

(define-condition unknown-name-error (process-error)
  ((name :initarg :name :reader unknown-name-error-name))
  (:report (lambda (c stream)
             (format stream "no process is registered as ~S" (unknown-name-error-name c)))))

(define-condition name-taken-error (process-error)
  ((name :initarg :name :reader name-taken-error-name))
  (:report (lambda (c stream)
             (format stream "~S is already registered as ~S"
                     (name-taken-error-name c) (process-error-process c)))))

(define-condition process-exit-signal (serious-condition)
  ((from :initarg :from :initform nil :reader process-exit-signal-from)
   (reason :initarg :reason :reader process-exit-signal-reason))
  (:documentation "Signaled in a process to make it exit.  Not an ERROR,
so that handlers for errors do not intercept it.")
  (:report (lambda (c stream)
             (format stream "exit signal ~S~@[ from ~S~]"
                     (process-exit-signal-reason c) (process-exit-signal-from c)))))

(define-condition call-timeout (process-error) ()
  (:report (lambda (c stream)
             (format stream "no reply from ~S in time" (process-error-process c)))))

(define-condition call-failed (process-error)
  ((reason :initarg :reason :reader call-failed-reason))
  (:report (lambda (c stream)
             (format stream "~S exited during a call: ~S"
                     (process-error-process c) (call-failed-reason c)))))

;;; --- Descriptors ---

(defstruct (process (:constructor %make-process) (:copier nil)
                    (:predicate process-p))
  (id 0 :type fixnum)
  (name nil :type symbol)
  (function nil :type (or function symbol))
  ;; A global copy of the arguments, consumed when the process starts.
  (arguments nil :type list)
  (fiber nil)
  (heap nil)
  (carrier nil)
  ;; The process that spawned this one, if any.
  (parent nil)
  (stack-size *default-process-stack-size* :type fixnum)
  (binding-stack-size sb-fiber:*default-fiber-binding-stack-size* :type fixnum)
  (state :new :type (member :new :runnable :running :waiting :exited))
  (lock (make-mutex :name "process"))
  ;; The carrier sets PARKED once a waiting process has actually been
  ;; suspended; a wake arriving before that sets WAKE-PENDING instead.
  (parked nil :type boolean)
  (wake-pending nil :type boolean)
  ;; The mailbox of a process without a heap; a heap process uses the
  ;; heap's own.
  (mailbox nil :type (or null fifo))
  (links nil :type list)
  ;; (ref . watcher) for each process watching this one, and
  ;; (ref . target) for each process this one watches.
  (monitors nil :type list)
  (monitoring nil :type list)
  (trap-exits nil :type boolean)
  ;; (from . reason) once an exit signal has been accepted.
  (pending-exit nil :type list)
  (started nil :type boolean)
  (exit-reason nil)
  (exit-value nil)
  (exit-semaphore (make-semaphore :name "process exit"))
  (reductions 0 :type fixnum))

(defmethod print-object ((process process) stream)
  (print-unreadable-object (process stream :type t)
    (format stream "~D~@[ ~S~] ~(~A~)"
            (process-id process) (process-name process) (process-state process))))

(defstruct (counters (:copier nil))
  (process-id 0 :type sb-ext:word)
  (ref 0 :type sb-ext:word))

(sb-ext:define-load-time-global *counters* (make-counters))

;;; Every process that has not been released, by id.
(sb-ext:define-load-time-global *processes* (make-hash-table :synchronized t))

;;; Registered names.
(sb-ext:define-load-time-global *registry* (make-hash-table :synchronized t :test 'eq))

(defun make-ref ()
  "A fresh reference: a fixnum unique for the life of the image."
  (sb-ext:atomic-incf (counters-ref *counters*)))

(declaim (inline current-process))
(defun current-process ()
  "The calling process, or NIL on a thread that is not running one."
  *current-process*)

(defun self ()
  "The calling process."
  (or *current-process*
      (error 'no-current-process-error :operation 'self)))

(defun process-alive-p (process)
  "True unless PROCESS has exited."
  (declare (type process process))
  (not (eq (process-state process) :exited)))

(defun list-processes ()
  "Every process that has not exited."
  (let ((result nil))
    (sb-ext:with-locked-hash-table (*processes*)
      (maphash (lambda (id p) (declare (ignore id))
                 (when (process-alive-p p) (push p result)))
               *processes*))
    (sort result #'< :key #'process-id)))

(defun resolve-process (designator &key (if-does-not-exist :error))
  "The process DESIGNATOR names: a process, or a registered name."
  (etypecase designator
    (process designator)
    (symbol (or (whereis designator)
                (ecase if-does-not-exist
                  (:error (error 'unknown-name-error :name designator))
                  ((nil) nil))))))

;;; --- Spawning ---

(defun make-process-heap (heap name)
  #-sb-process-heaps
  (when heap
    (error "process heaps are not available in this build"))
  #+sb-process-heaps
  (etypecase heap
    (null nil)
    ((eql t) (sb-fiber:make-heap :name (and name (string name))))
    (list (apply #'sb-fiber:make-heap :name (and name (string name)) heap))))

(defun spawn (function &key arguments name link monitor
                            (heap *default-process-heap*)
                            (stack-size *default-process-stack-size*)
                            (binding-stack-size sb-fiber:*default-fiber-binding-stack-size*)
                            trap-exits)
  "Start a process that applies FUNCTION to ARGUMENTS.  Returns the
process, and the monitor reference when MONITOR is true.

FUNCTION is a global function or the name of one; a closure allocated
in a process heap cannot be shared and is refused.  ARGUMENTS are
copied into the new process.  NAME registers the process.  LINK and
MONITOR link the caller to the process or make the caller monitor it.
HEAP is T for a heap of the process's own, NIL to share the global heap,
or a list of keyword arguments for SB-FIBER:MAKE-HEAP.  STACK-SIZE and
BINDING-STACK-SIZE size the process's fiber.  TRAP-EXITS makes exit
signals arrive as messages."
  (declare (type (or function symbol) function))
  (let ((parent *current-process*))
    (when (and (or link monitor) (not parent))
      (error 'no-current-process-error :operation 'spawn))
    (when (and name (not (symbolp name)))
      (error "a process name must be a symbol: ~S" name))
    #+sb-process-heaps
    (when (and (functionp function) (sb-fiber:object-heap function))
      (error 'sb-fiber:cross-heap-reference :object function))
    (ensure-scheduler)
    (let ((process
            (without-heap
              (%make-process
               :id (sb-ext:atomic-incf (counters-process-id *counters*))
               :name name
               :function function
               :arguments (sb-fiber:globalize arguments)
               :heap (make-process-heap heap name)
               :parent parent
               :mailbox (if heap nil (make-fifo))
               :stack-size stack-size
               :binding-stack-size binding-stack-size
               :trap-exits trap-exits))))
      (without-heap
        (setf (gethash (process-id process) *processes*) process))
      (when name
        (handler-case (register name process)
          (error (c)
            (remhash (process-id process) *processes*)
            (release-process process)
            (error c))))
      (when link (%link parent process))
      (let ((ref (when monitor (%monitor parent process))))
        (setf (process-state process) :runnable)
        (enqueue-process process)
        (values process ref)))))

(defun spawn-link (function &rest keys &key &allow-other-keys)
  "SPAWN with :LINK T."
  (apply #'spawn function :link t keys))

(defun spawn-monitor (function &rest keys &key &allow-other-keys)
  "SPAWN with :MONITOR T; returns the process and the monitor reference."
  (apply #'spawn function :monitor t keys))

;;; --- Running on a carrier ---

(defun release-process (process)
  "Free the fiber (and with it the heap) of an exited process."
  (let ((fiber (shiftf (process-fiber process) nil)))
    (when fiber
      (sb-fiber:release-fiber fiber))
    #+sb-process-heaps
    (let ((heap (process-heap process)))
      ;; A process that never got a fiber still has a heap to release.
      (when (and heap (not (sb-fiber:heap-fiber heap)))
        (sb-fiber:release-heap heap)))
    (remhash (process-id process) *processes*)))

(defun ensure-fiber (process)
  (let ((fiber (process-fiber process)))
    (unless fiber
      (setf fiber (sb-fiber:make-fiber #'process-main
                                       :name (format nil "process ~D" (process-id process))
                                       :stack-size (process-stack-size process)
                                       :binding-stack-size (process-binding-stack-size process)
                                       :heap (process-heap process))
            (process-fiber process) fiber))
    (unless (eq (sb-fiber:fiber-thread fiber) sb-thread:*current-thread*)
      (sb-fiber:fiber-migrate fiber sb-thread:*current-thread*))
    fiber))

(defun run-process (carrier process)
  (let ((fiber (ensure-fiber process)))
    (setf (process-carrier process) carrier)
    (with-mutex ((process-lock process))
      (setf (process-state process) :running))
    (setf (carrier-current carrier) process)
    (incf (carrier-switches carrier))
    (unwind-protect
         (sb-fiber:resume-fiber fiber)
      (setf (carrier-current carrier) nil))
    (after-run process)))

(defun after-run (process)
  "The process has suspended itself (or finished): act on the state it
left behind."
  (let ((action nil))
    (with-mutex ((process-lock process))
      (ecase (process-state process)
        (:runnable (setf action :enqueue))
        (:waiting
         (cond ((process-wake-pending process)
                (setf (process-wake-pending process) nil
                      (process-state process) :runnable
                      action :enqueue))
               (t (setf (process-parked process) t))))
        (:exited (setf action :release))
        (:running
         ;; The entry function escaped past our handlers; treat it as a crash.
         (setf (process-state process) :exited
               (process-exit-reason process) '(:error "escaped")
               action :release))))
    (case action
      (:enqueue (enqueue-process process))
      (:release (release-process process)))))

(defun wake-process (process)
  "Make a waiting PROCESS runnable."
  (let ((enqueue nil))
    (with-mutex ((process-lock process))
      (when (eq (process-state process) :waiting)
        (if (process-parked process)
            (setf (process-parked process) nil
                  (process-state process) :runnable
                  enqueue t)
            (setf (process-wake-pending process) t))))
    (when enqueue (enqueue-process process))))

;;; --- The process body ---

(defvar *saved-messages* nil
  "Bound in each process to a box holding the messages a selective
receive has skipped, oldest first.")

(defun error-reason (condition)
  (list :error condition))

(defun globalize-reason (reason)
  "A copy of REASON that no process heap owns."
  (without-heap
    (handler-case (sb-fiber:globalize reason)
      (error ()
        (let ((text (handler-case (princ-to-string reason)
                      (error () "unprintable reason"))))
          (if (and (consp reason) (eq (car reason) :error))
              (list :error text)
              text))))))

(defun report-process-error (process condition)
  (when *report-process-errors*
    (without-heap
      (ignore-errors
       (format *error-output* "~&;; ~S exited with error: ~A~%" process condition)
       (when (eq *report-process-errors* :backtrace)
         (sb-debug:print-backtrace :stream *error-output* :count 20))
       (finish-output *error-output*)))))

(defun check-pending-exit (process)
  "Exit if an exit signal has been accepted since the last check."
  (let ((pending (process-pending-exit process)))
    (when pending
      (error 'process-exit-signal :from (car pending) :reason (cdr pending)))))

(defun copy-arguments-in (process arguments)
  (declare (ignorable process))
  #+sb-process-heaps
  (if (process-heap process)
      (sb-fiber:copy-for-transfer arguments)
      arguments)
  #-sb-process-heaps
  arguments)

(defun process-main ()
  "Entry function of every process fiber."
  (let* ((process (carrier-current *carrier*))
         (*current-process* process)
         (*saved-messages* (list nil))
         (reason :normal)
         (value nil))
    (unwind-protect
         (catch 'process-exit
           (handler-bind ((process-exit-signal
                            (lambda (c)
                              (setf reason (process-exit-signal-reason c))
                              (throw 'process-exit nil)))
                          (serious-condition
                            (lambda (c)
                              (setf reason (error-reason c))
                              (report-process-error process c)
                              (throw 'process-exit nil))))
             (with-mutex ((process-lock process))
               (setf (process-started process) t))
             (check-pending-exit process)
             (let ((arguments (copy-arguments-in process (process-arguments process))))
               (setf (process-arguments process) nil)
               (setf value (multiple-value-list
                            (apply (process-function process) arguments))))))
      (finish-process process reason value))
    nil))

(defun finish-process (process reason value)
  "Runs in the exiting process: publish the exit, notify monitors and
links, and release the name."
  (let ((reason (globalize-reason reason))
        (value (without-heap
                 (handler-case (sb-fiber:globalize value)
                   (error () nil))))
        (monitors nil)
        (monitoring nil)
        (links nil))
    (without-heap
      (with-mutex ((process-lock process))
        (setf (process-state process) :exited
              (process-exit-reason process) reason
              (process-exit-value process) value
              monitors (shiftf (process-monitors process) nil)
              monitoring (shiftf (process-monitoring process) nil)
              links (shiftf (process-links process) nil)))
      (when (process-name process)
        (unregister-process process))
      (dolist (entry monitors)
        (let ((ref (car entry)) (watcher (cdr entry)))
          (with-mutex ((process-lock watcher))
            (setf (process-monitoring watcher)
                  (delete ref (process-monitoring watcher) :key #'car)))
          (%send watcher (list :down ref process reason))))
      (dolist (entry monitoring)
        (let ((ref (car entry)) (target (cdr entry)))
          (with-mutex ((process-lock target))
            (setf (process-monitors target)
                  (delete ref (process-monitors target) :key #'car)))))
      (dolist (other links)
        (with-mutex ((process-lock other))
          (setf (process-links other) (delete process (process-links other))))
        (deliver-exit-signal other process reason))
      (signal-semaphore (process-exit-semaphore process) (ash 1 20)))))

;;; --- Exit signals ---

(defun deliver-exit-signal (target from reason)
  "Deliver an exit signal to TARGET: a message if it traps exits, else
its death unless REASON is :NORMAL.  Called with no locks held."
  (let ((action nil) (fiber nil))
    (with-mutex ((process-lock target))
      (unless (eq (process-state target) :exited)
        (cond ((and (process-trap-exits target) (not (eq reason :kill)))
               (setf action :message))
              ((and (eq reason :normal) (not (eq from target)))
               nil)
              (t
               (unless (process-pending-exit target)
                 (setf (process-pending-exit target)
                       (without-heap (cons from (if (eq reason :kill) :killed reason)))))
               (setf fiber (and (process-started target) (process-fiber target))
                     action :kill)))))
    (ecase action
      ((nil))
      (:message (%send target (without-heap (list :exit from reason))))
      (:kill
       (when (and fiber (sb-fiber:fiber-alive-p fiber))
         (let ((pending (process-pending-exit target)))
           (handler-case
               (sb-fiber:interrupt-fiber
                fiber (without-heap
                        (make-condition 'process-exit-signal
                                        :from (car pending) :reason (cdr pending))))
             (sb-fiber:fiber-error () nil))))
       (wake-process target)))))

(defun signal-exit (target reason)
  "Send an exit signal with REASON to TARGET (a process or a name) on
behalf of the calling process, if any.  :KILL cannot be trapped; a
:NORMAL signal from another process is ignored."
  (let ((process (resolve-process target)))
    (deliver-exit-signal process *current-process* reason)
    t))

(defun exit-process (&optional (reason :normal))
  "Exit the calling process with REASON."
  (let ((process (self)))
    (error 'process-exit-signal :from process :reason reason)))

(defun trap-exits ()
  "Whether the calling process receives exit signals as (:EXIT FROM
REASON) messages instead of exiting."
  (process-trap-exits (self)))

(defun (setf trap-exits) (enable)
  (setf (process-trap-exits (self)) (and enable t)))

;;; --- Links and monitors ---

(defun lock-pair (a b thunk)
  "Call THUNK holding the locks of processes A and B in id order."
  (declare (function thunk))
  (if (< (process-id a) (process-id b))
      (with-mutex ((process-lock a)) (with-mutex ((process-lock b)) (funcall thunk)))
      (with-mutex ((process-lock b)) (with-mutex ((process-lock a)) (funcall thunk)))))

(defun %link (self target)
  (let ((dead nil))
    (without-heap
      (lock-pair self target
                 (lambda ()
                   (cond ((eq (process-state target) :exited)
                          (setf dead (process-exit-reason target)))
                         (t (pushnew target (process-links self))
                            (pushnew self (process-links target)))))))
    (when dead
      (deliver-exit-signal self target :noproc))
    t))

(defun link (target)
  "Link the calling process and TARGET: when either exits, the other
receives its exit signal.  Linking to a process that has exited delivers
an exit signal with reason :NOPROC."
  (%link (self) (resolve-process target)))

(defun unlink (target)
  "Remove the link between the calling process and TARGET."
  (let ((self (self)) (target (resolve-process target)))
    (lock-pair self target
               (lambda ()
                 (setf (process-links self) (delete target (process-links self))
                       (process-links target) (delete self (process-links target)))))
    t))

(defun %monitor (self target)
  (let ((ref (make-ref)) (dead nil))
    (without-heap
      (lock-pair self target
                 (lambda ()
                   (cond ((eq (process-state target) :exited)
                          (setf dead t))
                         (t (push (cons ref self) (process-monitors target))
                            (push (cons ref target) (process-monitoring self)))))))
    (when dead
      (%send self (without-heap (list :down ref target :noproc))))
    ref))

(defun monitor (target)
  "Watch TARGET: when it exits, the calling process receives (:DOWN REF
PROCESS REASON).  Returns REF.  Monitoring a process that has exited
delivers the message at once with reason :NOPROC."
  (%monitor (self) (resolve-process target)))

(defun demonitor (ref &key flush)
  "Stop watching the process REF refers to.  With FLUSH, also remove a
:DOWN message for REF from the mailbox."
  (let ((self (self)) (target nil))
    (with-mutex ((process-lock self))
      (let ((entry (assoc ref (process-monitoring self))))
        (when entry
          (setf target (cdr entry)
                (process-monitoring self) (delete entry (process-monitoring self))))))
    (when target
      (with-mutex ((process-lock target))
        (setf (process-monitors target)
              (delete ref (process-monitors target) :key #'car))))
    (when flush
      (receive-matching (lambda (m) (and (consp m) (eq (car m) :down) (eql (cadr m) ref)))
                        :timeout 0))
    (and target t)))

;;; --- Registry ---

(defun register (name process)
  "Register PROCESS under NAME, a symbol."
  (declare (type symbol name) (type process process))
  (when (null name)
    (error "NIL is not a valid process name"))
  (without-heap
    (sb-ext:with-locked-hash-table (*registry*)
      (let ((old (gethash name *registry*)))
        (when (and old (process-alive-p old) (not (eq old process)))
          (error 'name-taken-error :name name :process old)))
      (unless (process-alive-p process)
        (error 'dead-process-error :process process))
      (setf (process-name process) name
            (gethash name *registry*) process)))
  process)

(defun unregister (name)
  "Remove the registration of NAME.  Returns the process it named, or NIL."
  (declare (type symbol name))
  (sb-ext:with-locked-hash-table (*registry*)
    (let ((process (gethash name *registry*)))
      (when process
        (remhash name *registry*)
        (setf (process-name process) nil))
      process)))

(defun unregister-process (process)
  (sb-ext:with-locked-hash-table (*registry*)
    (let ((name (process-name process)))
      (when (and name (eq (gethash name *registry*) process))
        (remhash name *registry*)))))

(defun whereis (name)
  "The live process registered as NAME, or NIL."
  (declare (type symbol name))
  (let ((process (gethash name *registry*)))
    (and process (process-alive-p process) process)))

(defun registered ()
  "The registered names."
  (let ((names nil))
    (sb-ext:with-locked-hash-table (*registry*)
      (maphash (lambda (name process)
                 (when (process-alive-p process) (push name names)))
               *registry*))
    names))

;;; --- Mailboxes ---

(defun %send (process message)
  "Put MESSAGE in PROCESS's mailbox and wake it.  MESSAGE is copied into
the process's heap, or made global for a process without one."
  (unless (eq (process-state process) :exited)
    (cond ((process-heap process)
           (handler-case (sb-fiber:send-message (process-heap process) message)
             (sb-fiber:dead-heap-error () (return-from %send nil))))
          (t
           (let ((message #+sb-process-heaps (sb-fiber:globalize message)
                          #-sb-process-heaps message))
             (without-heap
               (with-mutex ((process-lock process))
                 (fifo-push (process-mailbox process) message))))))
    (wake-process process)
    t))

(defun send (target message)
  "Send MESSAGE to TARGET, a process or a registered name, and return
MESSAGE.  The message is copied into the target's heap.  Sending to a
process that has exited does nothing."
  (let ((process (resolve-process target)))
    (%send process message)
    (let ((self *current-process*))
      (when self (count-reduction self)))
    message))

(defun mailbox-count (&optional (process (self)))
  "Number of messages waiting for PROCESS.  Messages a selective receive
has skipped are counted only when PROCESS is the calling process."
  (declare (type process process))
  (+ (if (eq process *current-process*) (length (car *saved-messages*)) 0)
     (cond ((eq (process-state process) :exited) 0)
           ((process-heap process) (sb-fiber:heap-mailbox-count (process-heap process)))
           (t (with-mutex ((process-lock process)) (fifo-count (process-mailbox process)))))))

(defun mailbox-nonempty-p (process)
  "Caller holds the process lock."
  (if (process-heap process)
      (plusp (sb-fiber:heap-mailbox-count (process-heap process)))
      (not (fifo-empty-p (process-mailbox process)))))

(defun %mailbox-pop (process)
  "Take the oldest message that has arrived; (VALUES MESSAGE T) or NIL."
  (if (process-heap process)
      (sb-fiber:receive-message :heap (process-heap process))
      (with-mutex ((process-lock process))
        (fifo-pop (process-mailbox process)))))

(defun count-reduction (process)
  (when (> (incf (process-reductions process)) *reductions-per-slice*)
    (setf (process-reductions process) 0)
    (process-yield)))

(defun %wait (process timer)
  "Suspend PROCESS until something arrives: a message, an exit signal,
or the firing of TIMER."
  (let ((ready nil))
    (with-mutex ((process-lock process))
      (if (or (mailbox-nonempty-p process)
              (process-pending-exit process)
              (and timer (timer-fired timer)))
          (setf ready t)
          (setf (process-state process) :waiting)))
    (unless ready
      (sb-fiber:yield-fiber))))

(defun wake-timer (timer)
  (wake-process (timer-argument timer)))

(defun saved-push (message)
  (let ((box *saved-messages*))
    (setf (car box) (nconc (car box) (list message)))))

(defun saved-take (predicate)
  "Remove and return the oldest skipped message satisfying PREDICATE."
  (let* ((box *saved-messages*)
         (cell (if predicate
                   (member-if predicate (car box))
                   (car box))))
    (when cell
      (let ((message (car cell)))
        (setf (car box) (delete message (car box) :test #'eq :count 1))
        (values message t)))))

(defun receive-matching (predicate &key timeout)
  "Return the oldest message in the calling process's mailbox that
satisfies PREDICATE (or any message if it is NIL), and T, removing it.
Messages that do not match stay in order for later receives.  Waits
until such a message arrives; with TIMEOUT (seconds), gives up after
that long and returns NIL, NIL.  A timeout of 0 does not wait."
  (declare (type (or null function) predicate))
  (let ((process (self)))
    (check-pending-exit process)
    (count-reduction process)
    (multiple-value-bind (message found) (saved-take predicate)
      (when found (return-from receive-matching (values message t))))
    (flet ((drain ()
             (loop
               (multiple-value-bind (message found) (%mailbox-pop process)
                 (cond ((not found) (return nil))
                       ((or (null predicate) (funcall predicate message))
                        (return-from receive-matching (values message t)))
                       (t (saved-push message)))))))
      (drain)
      (when (and timeout (<= timeout 0))
        (return-from receive-matching (values nil nil)))
      (let ((timer (when timeout (schedule-timer timeout #'wake-timer process))))
        (unwind-protect
             (loop
               (%wait process timer)
               (check-pending-exit process)
               (drain)
               (when (and timer (timer-fired timer))
                 (return (values nil nil))))
          (when timer (cancel-timer timer)))))))

(defun receive-message (&key timeout)
  "Return the oldest message in the calling process's mailbox, and T;
wait for one, or with TIMEOUT (seconds), give up and return NIL, NIL."
  (receive-matching nil :timeout timeout))

(defmacro receive ((var &key timeout) &body clauses)
  "Selective receive.  Each clause is (TEST . BODY): the oldest message
for which some TEST, evaluated with VAR bound to the message, is true
is removed from the mailbox and the BODY of the first clause whose TEST
it satisfies runs with VAR bound to it.  A clause (:TIMEOUT . BODY) runs
instead if no such message arrives within TIMEOUT seconds; without one,
a timed-out RECEIVE returns NIL."
  (let* ((timeout-clause (find :timeout clauses :key #'car))
         (clauses (remove timeout-clause clauses))
         (found (gensym "FOUND"))
         (predicate (gensym "PREDICATE")))
    `(flet ((,predicate (,var)
              (declare (ignorable ,var))
              (or ,@(mapcar #'car clauses))))
       (declare (dynamic-extent #',predicate))
       (multiple-value-bind (,var ,found)
           (receive-matching #',predicate :timeout ,timeout)
         (declare (ignorable ,var))
         (if ,found
             (cond ,@clauses)
             (progn ,@(cdr timeout-clause)))))))

(defun flush-mailbox ()
  "Discard every message in the calling process's mailbox; returns how many."
  (let ((n 0))
    (loop (multiple-value-bind (message found) (receive-matching nil :timeout 0)
            (declare (ignore message))
            (unless found (return))
            (incf n)))
    n))

;;; --- Yielding, sleeping, waiting ---

(defun process-yield ()
  "Let other processes run.  Does nothing outside a process."
  (let ((process *current-process*))
    (when process
      (check-pending-exit process)
      (with-mutex ((process-lock process))
        (setf (process-state process) :runnable))
      (sb-fiber:yield-fiber)
      (check-pending-exit process))))

(defun process-sleep (seconds)
  "Suspend the calling process for SECONDS; messages that arrive
meanwhile wait in the mailbox."
  (let* ((process (self))
         (timer (schedule-timer seconds #'wake-timer process)))
    (unwind-protect
         (loop
           (let ((ready nil))
             (with-mutex ((process-lock process))
               (if (or (timer-fired timer) (process-pending-exit process))
                   (setf ready t)
                   (setf (process-state process) :waiting)))
             (unless ready (sb-fiber:yield-fiber)))
           (check-pending-exit process)
           (when (timer-fired timer) (return)))
      (cancel-timer timer))
    nil))

(defun join-process (process &key timeout)
  "Wait, on a thread that is not a process, for PROCESS to exit.
Returns its exit reason, its exit value (the values of its function as
a list, copied out of its heap) and T; or NIL, NIL, NIL after TIMEOUT
seconds."
  (declare (type process process))
  (when *current-process*
    (error "JOIN-PROCESS would block the carrier; use AWAIT-PROCESS in a process"))
  (if (wait-on-semaphore (process-exit-semaphore process) :timeout timeout)
      (values (process-exit-reason process) (process-exit-value process) t)
      (values nil nil nil)))

(defun await-process (process &key timeout)
  "Wait in the calling process for PROCESS to exit, by monitoring it.
Returns its exit reason and T, or NIL, NIL after TIMEOUT seconds."
  (let ((ref (monitor process)))
    (receive (m :timeout timeout)
      ((and (consp m) (eq (car m) :down) (eql (cadr m) ref))
       (values (fourth m) t))
      (:timeout (demonitor ref :flush t) (values nil nil)))))

;;; --- Timers ---

(defun fire-send-after (timer)
  (let ((target (car (timer-argument timer)))
        (message (cdr (timer-argument timer))))
    (let ((process (resolve-process target :if-does-not-exist nil)))
      (when process (%send process message)))))

(defun send-after (seconds target message)
  "Send MESSAGE to TARGET after SECONDS.  Returns a timer for
CANCEL-TIMER.  TARGET may be a name, resolved when the timer fires."
  (let ((message (without-heap (sb-fiber:globalize message))))
    (schedule-timer seconds #'fire-send-after
                    (without-heap (cons (if (process-p target) target (resolve-process target))
                                        message)))))

;;; --- Introspection ---

(defun process-info (process)
  "A plist describing PROCESS."
  (declare (type process process))
  (let ((heap (process-heap process)))
    (list :id (process-id process)
          :name (process-name process)
          :state (process-state process)
          :carrier (let ((c (process-carrier process))) (and c (carrier-index c)))
          :messages (mailbox-count process)
          :links (with-mutex ((process-lock process)) (copy-list (process-links process)))
          :monitors (with-mutex ((process-lock process))
                      (mapcar #'cdr (process-monitors process)))
          :trap-exits (process-trap-exits process)
          :reductions (process-reductions process)
          :exit-reason (process-exit-reason process)
          :heap heap
          #+sb-process-heaps
          :heap-bytes
          #+sb-process-heaps
          (and heap (sb-fiber:heap-alive-p heap) (sb-fiber:heap-bytes-allocated heap))
          :stack-size (process-stack-size process))))

;;; --- Shutdown ---

(defun kill-all-processes (timeout)
  (let ((processes (list-processes)))
    (dolist (p processes)
      (deliver-exit-signal p nil :kill))
    (let ((deadline (+ (now) (seconds->units timeout))))
      (loop while (and (some #'process-alive-p processes) (< (now) deadline))
            do (sleep 0.01)))))

(defun release-all-processes ()
  "After the carriers have stopped: release whatever is left."
  (let ((processes nil))
    (sb-ext:with-locked-hash-table (*processes*)
      (maphash (lambda (id p) (declare (ignore id)) (push p processes)) *processes*))
    (dolist (p processes)
      (with-mutex ((process-lock p))
        (unless (eq (process-state p) :exited)
          (setf (process-state p) :exited
                (process-exit-reason p) :killed)))
      (unregister-process p)
      (let ((fiber (process-fiber p)))
        (when (and fiber (eq (sb-fiber:fiber-state fiber) :running))
          ;; Cannot happen once the carriers have exited, but never release
          ;; a running fiber.
          (setf (process-fiber p) nil)))
      (release-process p)
      (signal-semaphore (process-exit-semaphore p) (ash 1 20)))))

(defun run-process-reporting (carrier process)
  "RUN-PROCESS, keeping the carrier alive if the scheduler itself fails."
  (handler-case (run-process carrier process)
    (serious-condition (c)
      (ignore-errors
       (format *error-output* "~&;; sb-process: carrier ~D failed running ~S: ~A~%"
               (carrier-index carrier) process c)
       (finish-output *error-output*))
      (with-mutex ((process-lock process))
        (unless (eq (process-state process) :exited)
          (setf (process-state process) :exited
                (process-exit-reason process) (list :error (princ-to-string c)))))
      (signal-semaphore (process-exit-semaphore process) (ash 1 20)))))
