;;;; Fibers with heaps under a pool of carrier threads: message passing,
;;;; waking and cancellation with the fiber suspended on one thread and
;;;; the sender on another.
;;;;
;;;; The pool below is the least scheduler that exercises the primitives
;;;; a process layer is built on: a fiber parks by yielding to its
;;;; carrier and is resumed when something wakes it, a send is a copy into
;;;; the mailbox followed by a wake, a kill is INTERRUPT-FIBER followed by
;;;; a wake, and stopping the pool releases the fibers it still holds.  A
;;;; real scheduler lives outside SBCL; this one is here so the runtime's
;;;; side of those interactions stays covered.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

(unless (and (member :sb-thread *features*)
             (member :sb-local-heaps *features*)
             (member :sb-fiber *features*))
  (invoke-restart 'run-tests::skip-file))

(require :sb-fiber)
(use-package :sb-fiber)

;;; --- The pool ---

(defstruct (carrier (:constructor %make-carrier))
  (lock (sb-thread:make-mutex :name "carrier"))
  (queue (sb-thread:make-waitqueue))
  ;; Tasks to resume, oldest last.
  (runnable nil)
  ;; Every task this carrier has run, for release at stop.
  (tasks nil)
  (stop nil)
  (thread nil))

(defstruct (task (:constructor %make-task))
  (carrier nil)
  ;; The heap is made at spawn, so a message sent before the task has
  ;; run has a mailbox to land in; the fiber is made by the carrier
  ;; that will resume it, and owns the heap from then on.
  (heap nil)
  (fiber nil)
  (function nil)
  (arguments nil)
  ;; :NEW until first run; :RUNNABLE while queued or running; :WAITING
  ;; while parked; :DONE.  Changed only under the carrier's lock, except
  ;; the final transition, which the task makes on its own fiber.
  (state :new)
  (values nil)
  (condition nil)
  (done (sb-thread:make-semaphore :name "task done")))

(defvar *pool* nil "The carriers, as a vector.")
(defvar *task* nil "The task running on this carrier, bound in its fiber.")
(defvar *next-carrier* 0)

(defun mailbox-nonempty-p (heap)
  (plusp (heap-mailbox-count heap)))

(defun enqueue (carrier task)
  "Queue TASK on CARRIER.  Called with the carrier's lock held; the cell
is global because the queue is."
  (setf (task-state task) :runnable)
  (without-heap
    (setf (carrier-runnable carrier)
          (nconc (carrier-runnable carrier) (list task))))
  (sb-thread:condition-broadcast (carrier-queue carrier)))

(defun wake (task)
  "Make TASK runnable if it is waiting.  From any thread, any heap."
  (let ((carrier (task-carrier task)))
    (sb-thread:with-mutex ((carrier-lock carrier))
      (when (eq (task-state task) :waiting)
        (enqueue carrier task)))
    t))

(defun park-unless (test heap)
  "Suspend the calling task until WAKE, unless (TEST HEAP) is already
true.  The test runs under the carrier's lock, so a wake that lands
between it and the suspension is not lost: the waker either sees
:WAITING and queues the task, or made TEST true before it ran."
  (let* ((task *task*)
         (carrier (task-carrier task))
         (park (sb-thread:with-mutex ((carrier-lock carrier))
                 (unless (funcall test heap)
                   (setf (task-state task) :waiting)
                   t))))
    (when park
      (yield-fiber))))

(defun task-yield ()
  "Let the other tasks on this carrier run."
  (let* ((task *task*)
         (carrier (task-carrier task)))
    (sb-thread:with-mutex ((carrier-lock carrier))
      (enqueue carrier task))
    (yield-fiber)))

(defun receive ()
  "The next message for the calling task, waiting for one."
  (let ((heap (current-heap)))
    (loop
      (multiple-value-bind (message found) (receive-message)
        (when found (return message)))
      (park-unless #'mailbox-nonempty-p heap))))

(defun send (task message)
  "Copy MESSAGE into TASK's mailbox and wake it."
  (send-message (task-heap task) message)
  (wake task))

(defun kill (task condition)
  "Stage CONDITION in TASK's fiber and wake it.  From any thread."
  (interrupt-fiber (task-fiber task) condition)
  (wake task))

(defun run-task (task)
  "The fiber's entry function: apply the task's function and record the
outcome, copying it out of the heap before storing it in the global
task struct."
  (let ((*task* task))
    (handler-case
        (let ((values (multiple-value-list (apply (task-function task)
                                                  (task-arguments task)))))
          (without-heap
            (setf (task-values task) (globalize values))))
      (serious-condition (c)
        (without-heap
          (setf (task-condition task)
                (handler-case (globalize c)
                  (error () (princ-to-string c)))))))
    (without-heap
      (setf (task-state task) :done))))

(defun carrier-run-one (carrier task)
  (unless (task-fiber task)
    ;; Made here, on the thread that resumes it.  The entry closure is
    ;; global because this thread has no heap installed.
    (setf (task-fiber task)
          (make-fiber (lambda () (run-task task)) :heap (task-heap task)))
    (push task (carrier-tasks carrier)))
  (resume-fiber (task-fiber task))
  (when (eq (task-state task) :done)
    (release-fiber (task-fiber task))
    (sb-thread:signal-semaphore (task-done task) 1000)))

(defun carrier-loop (carrier)
  (with-fiber-thread ()
    (loop
      (let ((task (sb-thread:with-mutex ((carrier-lock carrier))
                    (loop until (or (carrier-runnable carrier)
                                    (carrier-stop carrier))
                          do (sb-thread:condition-wait (carrier-queue carrier)
                                                       (carrier-lock carrier)))
                    (pop (carrier-runnable carrier)))))
        (cond (task (carrier-run-one carrier task))
              (t (return)))))
    ;; Whatever is still parked goes with the pool: a suspended fiber is
    ;; released from the thread that owns it, heap, mailbox and all.
    (dolist (task (carrier-tasks carrier))
      (let ((fiber (task-fiber task)))
        (when (and fiber (fiber-alive-p fiber))
          (release-fiber fiber))))))

(defun start-pool (n)
  (setf *next-carrier* 0
        *pool* (coerce (loop for i below n
                             collect (let ((carrier (%make-carrier)))
                                       (setf (carrier-thread carrier)
                                             (sb-thread:make-thread
                                              (lambda () (carrier-loop carrier))
                                              :name (format nil "carrier-~D" i)))
                                       carrier))
                       'vector)))

(defun stop-pool ()
  (loop for carrier across *pool*
        do (sb-thread:with-mutex ((carrier-lock carrier))
             (setf (carrier-stop carrier) t)
             (sb-thread:condition-broadcast (carrier-queue carrier))))
  (loop for carrier across *pool*
        do (sb-thread:join-thread (carrier-thread carrier)))
  (setf *pool* nil))

(defmacro with-pool ((n) &body body)
  `(progn (start-pool ,n)
          (unwind-protect (progn ,@body)
            (stop-pool))))

(defun spawn (function &rest arguments)
  "Start FUNCTION on the next carrier.  FUNCTION is global and the
arguments are copied out of whatever heap the caller has installed, as
the struct is."
  (let* ((carrier (aref *pool* (mod (incf *next-carrier*) (length *pool*))))
         (task (without-heap
                 (%make-task :carrier carrier :function function
                             :heap (make-heap)
                             :arguments (globalize arguments)))))
    (sb-thread:with-mutex ((carrier-lock carrier))
      (enqueue carrier task))
    task))

(defun join (task &optional (timeout 10))
  "The task's values and T, or NIL and NIL after TIMEOUT."
  (if (sb-thread:wait-on-semaphore (task-done task) :timeout timeout)
      (values (task-values task) t)
      (values nil nil)))

(defun wait-until (predicate &optional (seconds 5))
  (loop repeat (ceiling seconds 0.01)
        until (funcall predicate)
        do (sleep 0.01))
  (funcall predicate))

;;; --- Tasks ---

(defun ponger (rounds)
  (dotimes (i rounds :ponged)
    (let ((message (receive)))
      (send (first message) (list :pong (second message))))))

(defun pinger (pong rounds)
  (dotimes (i rounds rounds)
    (send pong (list *task* i))
    (let ((reply (receive)))
      (assert (equal reply (list :pong i))))))

(defun idler ()
  (receive))

(defun holder ()
  (let ((message (receive)))
    (setf (car message) :mutated)
    (send (second message) :done)
    :held))

(defun mutator (holder)
  (let ((message (list :original *task*)))
    (send holder message)
    (receive)
    (car message)))

(defun allocator (n)
  (let ((keep nil))
    (dotimes (i n)
      (push (make-string 100 :initial-element #\x) keep)
      (when (zerop (mod i 1000)) (task-yield)))
    (length keep)))

(defun never (heap)
  (declare (ignore heap))
  nil)

(defun sleeper ()
  (park-unless #'never (current-heap))
  :woken)

;;; --- Tests ---

(with-test (:name (:fiber :carriers :ping-pong-across-carriers))
  (with-pool (4)
    (let* ((pongs (loop repeat 20 collect (spawn 'ponger 200)))
           (pings (loop for pong in pongs collect (spawn 'pinger pong 200))))
      (dolist (ping pings)
        (multiple-value-bind (values done) (join ping)
          (assert done)
          (assert (equal values '(200)))))
      (dolist (pong pongs)
        (multiple-value-bind (values done) (join pong)
          (assert done)
          (assert (equal values '(:ponged)))))))
  (assert (null (verify-all-heaps))))

(with-test (:name (:fiber :carriers :kill-parked-fiber))
  (with-pool (2)
    (let ((task (spawn 'idler)))
      (assert (wait-until (lambda () (eq (task-state task) :waiting))))
      (let ((heap (task-heap task)))
        (assert (heap-alive-p heap))
        (kill task (make-condition 'simple-error :format-control "killed"))
        (multiple-value-bind (values done) (join task)
          (assert done)
          (assert (null values))
          (assert (typep (task-condition task) 'simple-error))
          (assert (null (object-heap (task-condition task)))))
        (assert (heap-released-p heap)))))
  (assert (null (verify-all-heaps))))

(with-test (:name (:fiber :carriers :messages-are-copies))
  (with-pool (2)
    (let* ((holder (spawn 'holder))
           (mutator (spawn 'mutator holder)))
      (multiple-value-bind (values done) (join mutator)
        (assert done)
        (assert (equal values '(:original))))
      (multiple-value-bind (values done) (join holder)
        (assert done)
        (assert (equal values '(:held))))))
  (assert (null (verify-all-heaps))))

(with-test (:name (:fiber :carriers :allocation-with-yields))
  (let ((*default-heap-gc-threshold* 200000))
    (with-pool (4)
      (let ((tasks (loop repeat 16 collect (spawn 'allocator 20000))))
        (dolist (task tasks)
          (multiple-value-bind (values done) (join task)
            (assert done)
            (assert (equal values '(20000))))))))
  (assert (null (verify-all-heaps))))

(with-test (:name (:fiber :carriers :stop-with-parked-fibers))
  (let ((heaps nil))
    (with-pool (3)
      (let ((tasks (loop repeat 5 collect (spawn 'sleeper))))
        (assert (wait-until (lambda ()
                              (every (lambda (task) (eq (task-state task) :waiting))
                                     tasks))))
        (dolist (task tasks)
          (push (task-heap task) heaps)
          ;; Queued but never received: the message is released with
          ;; the heap.
          (send-message (task-heap task) (list :pending (make-string 64))))
        (assert (every (lambda (heap) (= (heap-mailbox-count heap) 1)) heaps))))
    (assert (every #'heap-released-p heaps))
    (assert (null (verify-all-heaps)))))
