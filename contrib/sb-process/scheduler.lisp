;;;; -*-  Lisp -*-
;;;;
;;;; The scheduler: a pool of carrier threads, each running processes
;;;; from a run queue of its own and stealing from the others when idle,
;;;; and one timer heap shared by all of them.
;;;;
;;;; Everything here is global bookkeeping.  A process may own a heap,
;;;; and the scheduler's structures must never point into one, so every
;;;; function that allocates on behalf of a process does so inside
;;;; SB-FIBER:WITHOUT-HEAP.

(in-package :sb-process)

(defmacro without-heap (&body body)
  "Run BODY allocating in the global heap."
  `(sb-fiber:without-heap ,@body))

;;; --- FIFO ---

(defstruct (fifo (:constructor make-fifo ()) (:copier nil))
  (head nil :type list)
  (tail nil :type list)
  (count 0 :type fixnum))

(declaim (inline fifo-empty-p))
(defun fifo-empty-p (fifo)
  (zerop (fifo-count fifo)))

(defun fifo-push (fifo x)
  (push x (fifo-tail fifo))
  (incf (fifo-count fifo))
  x)

(defun fifo-pop (fifo)
  "Remove and return the oldest element, and T; or NIL, NIL."
  (when (and (null (fifo-head fifo)) (fifo-tail fifo))
    (setf (fifo-head fifo) (nreverse (fifo-tail fifo))
          (fifo-tail fifo) nil))
  (if (fifo-head fifo)
      (progn (decf (fifo-count fifo))
             (values (pop (fifo-head fifo)) t))
      (values nil nil)))

(defun fifo-list (fifo)
  (append (fifo-head fifo) (reverse (fifo-tail fifo))))

(defun fifo-delete (fifo x)
  (let ((before (fifo-count fifo)))
    (setf (fifo-head fifo) (delete x (fifo-head fifo) :count 1))
    (when (= (length (fifo-head fifo)) (- before (length (fifo-tail fifo))))
      (setf (fifo-tail fifo) (delete x (fifo-tail fifo) :count 1)))
    (setf (fifo-count fifo) (+ (length (fifo-head fifo)) (length (fifo-tail fifo))))
    (/= before (fifo-count fifo))))

;;; --- Time ---

(declaim (inline now))
(defun now ()
  "Monotonic time in internal time units."
  (get-internal-real-time))

(defun seconds->units (seconds)
  (values (ceiling (* seconds internal-time-units-per-second))))

(defun units->seconds (units)
  (/ units internal-time-units-per-second))

;;; --- Timers: a binary min-heap keyed by deadline ---

(defstruct (timer (:constructor %make-timer (deadline function argument))
                  (:copier nil))
  (deadline 0 :type (unsigned-byte 62))
  (function nil :type (or null function symbol))
  (argument nil)
  (cancelled nil :type boolean)
  (fired nil :type boolean))

(defmethod print-object ((timer timer) stream)
  (print-unreadable-object (timer stream :type t :identity t)
    (format stream "~:[~;fired ~]~:[~;cancelled ~]in ~,3Fs"
            (timer-fired timer) (timer-cancelled timer)
            (units->seconds (- (timer-deadline timer) (now))))))

(defun heap-swap (heap i j)
  (rotatef (aref heap i) (aref heap j)))

(defun heap-insert (heap timer)
  (vector-push-extend timer heap)
  (let ((i (1- (length heap))))
    (loop while (plusp i)
          do (let ((parent (ash (1- i) -1)))
               (if (< (timer-deadline (aref heap i))
                      (timer-deadline (aref heap parent)))
                   (progn (heap-swap heap i parent) (setf i parent))
                   (return))))))

(defun heap-pop (heap)
  (let ((top (aref heap 0))
        (last (vector-pop heap)))
    (when (plusp (length heap))
      (setf (aref heap 0) last)
      (let ((i 0) (n (length heap)))
        (loop
          (let* ((l (+ (* 2 i) 1)) (r (+ l 1)) (m i))
            (when (and (< l n) (< (timer-deadline (aref heap l)) (timer-deadline (aref heap m))))
              (setf m l))
            (when (and (< r n) (< (timer-deadline (aref heap r)) (timer-deadline (aref heap m))))
              (setf m r))
            (when (= m i) (return))
            (heap-swap heap i m)
            (setf i m)))))
    top))

;;; --- Carriers ---

(defstruct (carrier (:constructor %make-carrier (index)) (:copier nil))
  (index 0 :type fixnum)
  (thread nil)
  (lock (make-mutex :name "carrier"))
  (run-queue (make-fifo) :type fifo)
  ;; The process this carrier is running, if any.
  (current nil)
  (stop nil :type boolean)
  ;; Statistics.
  (switches 0 :type fixnum)
  (steals 0 :type fixnum))

(defmethod print-object ((carrier carrier) stream)
  (print-unreadable-object (carrier stream :type t)
    (format stream "~D" (carrier-index carrier))))

(defstruct (scheduler (:constructor %make-scheduler) (:copier nil))
  (carriers #() :type simple-vector)
  (lock (make-mutex :name "scheduler"))
  ;; Idle carriers wait here; ENQUEUE and timers notify it.
  (idle-queue (make-waitqueue :name "scheduler idle"))
  (idle-count 0 :type fixnum)
  (timers (make-array 16 :adjustable t :fill-pointer 0))
  (next-carrier 0 :type fixnum)
  (state :stopped :type (member :stopped :running :stopping)))

(sb-ext:define-load-time-global *scheduler* nil
  "The scheduler, once started.")

(defvar *carrier* nil
  "Bound on each carrier thread to its carrier.")

(defun default-carrier-count ()
  (let ((n (ignore-errors
            (sb-alien:alien-funcall
             (sb-alien:extern-alien "sysconf" (function sb-alien:long sb-alien:int))
             #+linux 84 #+darwin 58 #-(or linux darwin) -1))))
    (if (and n (plusp n)) (min n 64) 4)))

(defvar *default-carrier-count* nil
  "Number of carrier threads START-SCHEDULER creates when not told
otherwise; NIL means one per online CPU.")

(defun scheduler-running-p ()
  "True if the scheduler has been started and not stopped."
  (let ((s *scheduler*))
    (and s (eq (scheduler-state s) :running))))

(defun scheduler-carrier-count ()
  "The number of carrier threads of the running scheduler, or 0."
  (let ((s *scheduler*))
    (if s (length (scheduler-carriers s)) 0)))

(defun ensure-scheduler ()
  (or (and (scheduler-running-p) *scheduler*)
      (start-scheduler)))

(defun any-run-queue-nonempty-p (scheduler)
  (loop for c across (scheduler-carriers scheduler)
        thereis (not (fifo-empty-p (carrier-run-queue c)))))

(defun pick-carrier (scheduler)
  "A carrier for a process that has no home yet, round-robin."
  (let* ((carriers (scheduler-carriers scheduler))
         (i (with-mutex ((scheduler-lock scheduler))
              (let ((i (scheduler-next-carrier scheduler)))
                (setf (scheduler-next-carrier scheduler)
                      (mod (1+ i) (length carriers)))
                i))))
    (aref carriers i)))

(defun notify-idle-carrier (scheduler)
  (with-mutex ((scheduler-lock scheduler))
    (when (plusp (scheduler-idle-count scheduler))
      (condition-notify (scheduler-idle-queue scheduler)))))

;;; --- Timers API (scheduler side) ---

(defun schedule-timer (delay-seconds function argument)
  "Arrange for (FUNCALL FUNCTION TIMER) on some carrier once
DELAY-SECONDS have passed.  Returns the timer."
  (let* ((s (ensure-scheduler))
         (timer (without-heap
                  (%make-timer (+ (now) (seconds->units (max 0 delay-seconds)))
                               function argument)))
         (earliest nil))
    (without-heap
      (with-mutex ((scheduler-lock s))
        (let ((heap (scheduler-timers s)))
          (setf earliest (or (zerop (length heap))
                             (< (timer-deadline timer) (timer-deadline (aref heap 0)))))
          (heap-insert heap timer))
        ;; A carrier sleeping until the previous earliest deadline has to
        ;; recompute its wait.
        (when (and earliest (plusp (scheduler-idle-count s)))
          (condition-broadcast (scheduler-idle-queue s)))))
    timer))

(defun cancel-timer (timer)
  "Cancel TIMER.  Returns T if it had not fired yet."
  (declare (type timer timer))
  (let ((s *scheduler*))
    (if s
        (with-mutex ((scheduler-lock s))
          (cond ((or (timer-fired timer) (timer-cancelled timer)) nil)
                (t (setf (timer-cancelled timer) t))))
        nil)))

(defun timer-active-p (timer)
  (not (or (timer-fired timer) (timer-cancelled timer))))

(defun take-due-timers (scheduler)
  "Under the lock, pop every timer whose deadline has passed."
  (let ((due nil) (now (now)))
    (with-mutex ((scheduler-lock scheduler))
      (let ((heap (scheduler-timers scheduler)))
        (loop while (and (plusp (length heap))
                         (<= (timer-deadline (aref heap 0)) now))
              do (let ((timer (heap-pop heap)))
                   (unless (timer-cancelled timer)
                     (setf (timer-fired timer) t)
                     (push timer due))))))
    (nreverse due)))

(defun fire-due-timers (scheduler)
  (dolist (timer (take-due-timers scheduler))
    (funcall (timer-function timer) timer)))

(defun seconds-until-next-timer (scheduler)
  "Caller holds the scheduler lock.  NIL if there is no timer."
  (let ((heap (scheduler-timers scheduler)))
    (loop while (and (plusp (length heap)) (timer-cancelled (aref heap 0)))
          do (heap-pop heap))
    (when (plusp (length heap))
      (max 0 (units->seconds (- (timer-deadline (aref heap 0)) (now)))))))

;;; --- Run queues ---

(defun enqueue-process (process)
  "Make PROCESS runnable on its carrier (or a fresh one).  The caller
has set its state."
  (declare (notinline process-carrier (setf process-carrier)))
  (let* ((s (ensure-scheduler))
         (c (or (process-carrier process)
                (setf (process-carrier process) (pick-carrier s)))))
    (without-heap
      (with-mutex ((carrier-lock c))
        (fifo-push (carrier-run-queue c) process)))
    (notify-idle-carrier s)
    process))

(defun dequeue-process (carrier)
  (with-mutex ((carrier-lock carrier))
    (fifo-pop (carrier-run-queue carrier))))

(defun steal-process (carrier scheduler)
  (let ((carriers (scheduler-carriers scheduler)))
    (loop for i from 1 below (length carriers)
          for victim = (aref carriers (mod (+ (carrier-index carrier) i) (length carriers)))
          do (multiple-value-bind (process found) (dequeue-process victim)
               (when found
                 (incf (carrier-steals carrier))
                 (return process))))))

(defun next-process (carrier scheduler)
  (multiple-value-bind (process found) (dequeue-process carrier)
    (if found
        process
        (steal-process carrier scheduler))))

(defun carrier-idle (carrier scheduler)
  (with-mutex ((scheduler-lock scheduler))
    (incf (scheduler-idle-count scheduler))
    (unwind-protect
         (unless (or (carrier-stop carrier)
                     (any-run-queue-nonempty-p scheduler))
           (let ((timeout (seconds-until-next-timer scheduler)))
             (if (and timeout (zerop timeout))
                 nil
                 (condition-wait (scheduler-idle-queue scheduler)
                                 (scheduler-lock scheduler)
                                 :timeout timeout))))
      (decf (scheduler-idle-count scheduler)))))

(defun carrier-main (carrier)
  (let ((*carrier* carrier)
        (scheduler *scheduler*))
    (sb-fiber:with-fiber-thread (:name (format nil "carrier ~D" (carrier-index carrier)))
      (loop
        (fire-due-timers scheduler)
        (let ((process (next-process carrier scheduler)))
          (cond (process (run-process-reporting carrier process))
                ((carrier-stop carrier) (return))
                (t (carrier-idle carrier scheduler))))))))

;;; --- Lifecycle ---

(defun start-scheduler (&key (carriers (or *default-carrier-count* (default-carrier-count))))
  "Start the scheduler with CARRIERS carrier threads.  SPAWN starts one
on demand, so calling this is only needed to choose the carrier count."
  (declare (type (integer 1 1024) carriers))
  (when (scheduler-running-p)
    (error "the scheduler is already running"))
  (let ((s (without-heap
             (%make-scheduler
              :carriers (coerce (loop for i below carriers collect (%make-carrier i))
                                'simple-vector)
              :state :running))))
    (setf *scheduler* s)
    ;; Carriers are system threads: like the finalizer thread they are
    ;; not the user's to join, and they do not keep EXIT waiting.
    (loop for c across (scheduler-carriers s)
          do (setf (carrier-thread c)
                   (sb-thread::with-system-mutex (sb-thread::*make-thread-lock*)
                     (sb-thread::make-system-thread
                      (format nil "sb-process carrier ~D" (carrier-index c))
                      #'carrier-main (list c) nil))))
    s))

(defun stop-scheduler (&key (timeout 5))
  "Kill every process, wait up to TIMEOUT seconds for them to exit, and
stop the carrier threads.  Processes that never yield are abandoned
with their fibers released."
  (let ((s *scheduler*))
    (when (and s (eq (scheduler-state s) :running))
      (setf (scheduler-state s) :stopping)
      (kill-all-processes timeout)
      (loop for c across (scheduler-carriers s) do (setf (carrier-stop c) t))
      (with-mutex ((scheduler-lock s))
        (condition-broadcast (scheduler-idle-queue s)))
      (loop for c across (scheduler-carriers s)
            do (sb-thread:join-thread (carrier-thread c) :default nil :timeout timeout))
      (release-all-processes)
      (setf (scheduler-state s) :stopped)
      t)))
