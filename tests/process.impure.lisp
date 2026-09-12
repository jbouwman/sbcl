;;;; Tests for sb-process: the scheduler, processes, messaging, links,
;;;; monitors, the registry and timers.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

(unless (and (member :sb-thread *features*)
             (member :sb-fiber *features*))
  (invoke-restart 'run-tests::skip-file))

(require :sb-process)

(defpackage :sb-process-test
  (:use :cl :sb-process :test-util :assertoid))
(in-package :sb-process-test)

(setf *report-process-errors* nil)

(defun return-42 () 42)

(defmacro with-exit ((reason value &optional (done (gensym "DONE"))) form &body body)
  `(multiple-value-bind (,reason ,value ,done) (join-process ,form :timeout 10)
     (declare (ignorable ,reason ,value))
     (assert ,done () "~S did not exit in time" ',form)
     ,@body))

(defun wait-until (predicate &optional (seconds 5))
  (loop repeat (ceiling seconds 0.01)
        until (funcall predicate)
        do (sleep 0.01))
  (funcall predicate))

;;; --- Scheduler ---

(with-test (:name (:process :scheduler :start-stop))
  (assert (not (scheduler-running-p)))
  (start-scheduler :carriers 3)
  (assert (scheduler-running-p))
  (assert (= (scheduler-carrier-count) 3))
  (assert-error (start-scheduler) error)
  (assert (stop-scheduler))
  (assert (not (scheduler-running-p)))
  ;; SPAWN starts it again on demand.
  (with-exit (reason value) (spawn 'return-42)
    (assert (eq reason :normal))
    (assert (equal value '(42))))
  (assert (scheduler-running-p)))

;;; --- Spawning and exiting ---

(defun adder (a b) (+ a b))

(with-test (:name (:process :spawn :arguments-and-values))
  (with-exit (reason value) (spawn 'adder :arguments '(1 2))
    (assert (eq reason :normal))
    (assert (equal value '(3))))
  (with-exit (reason value) (spawn #'adder :arguments (list 40 2) :heap nil)
    (assert (eq reason :normal))
    (assert (equal value '(42)))))

(defun crasher () (error "boom"))
(defun quitter () (exit-process :bye) :not-reached)
(defun idler () (receive-message) :woke)

(with-test (:name (:process :spawn :exit-reasons))
  (with-exit (reason value) (spawn 'crasher)
    (assert (and (consp reason) (eq (car reason) :error)))
    (assert (typep (second reason) 'simple-error))
    (assert (null value)))
  (with-exit (reason value) (spawn 'quitter)
    (assert (eq reason :bye)))
  (let ((p (spawn 'idler)))
    (assert (wait-until (lambda () (eq (process-state p) :waiting))))
    (assert (process-alive-p p))
    (signal-exit p :kill)
    (with-exit (reason value) p
      (assert (eq reason :killed))
      (assert (not (process-alive-p p)))))
  ;; A :NORMAL signal from outside is ignored.
  (let ((p (spawn 'idler)))
    (assert (wait-until (lambda () (eq (process-state p) :waiting))))
    (signal-exit p :normal)
    (sleep 0.05)
    (assert (process-alive-p p))
    (send p :wake)
    (with-exit (reason value) p
      (assert (eq reason :normal))
      (assert (equal value '(:woke))))))

(with-test (:name (:process :spawn :closure-from-heap-rejected))
  (defun spawn-closure ()
    (let ((n (random 10)))
      (handler-case (progn (spawn (lambda () (incf n))) :spawned)
        (sb-fiber:cross-heap-reference () :rejected))))
  (with-exit (reason value) (spawn 'spawn-closure)
    (assert (eq reason :normal))
    (assert (equal value (list #+sb-process-heaps :rejected #-sb-process-heaps :spawned)))))

;;; --- Messages ---

(defun echo ()
  (loop (receive (m)
          ((and (consp m) (eq (car m) :echo)) (send (cadr m) (list :echoed (caddr m))))
          ((eq m :stop) (return :stopped)))))

(defun echo-client (echo)
  (send echo (list :echo (self) "hello"))
  (send echo (list :echo (self) "world"))
  (send (self) :noise)
  (let ((r2 (receive (m) ((and (consp m) (equal (cadr m) "world")) (cadr m))))
        (r1 (receive (m) ((and (consp m) (equal (cadr m) "hello")) (cadr m))))
        (n (receive-message :timeout 1)))
    (send echo :stop)
    (list r1 r2 n (mailbox-count))))

(with-test (:name (:process :messages :selective-receive))
  (let* ((e (spawn 'echo :name 'echo))
         (c (spawn 'echo-client :arguments (list e))))
    (assert (eq (whereis 'echo) e))
    (assert (member 'echo (registered)))
    (with-exit (reason value) c
      (assert (eq reason :normal))
      (assert (equal value '(("hello" "world" :noise 0)))))
    (with-exit (reason value) e
      (assert (eq reason :normal))
      (assert (equal value '(:stopped))))
    (assert (null (whereis 'echo)))))

(defun timeouts ()
  (list (multiple-value-list (receive-message :timeout 0))
        (multiple-value-list (receive-message :timeout 0.05))
        (receive (m :timeout 0.05) ((eq m :never) :no) (:timeout :timed-out))
        (progn (send (self) :a) (send (self) :b) (send (self) :c)
               (list (receive (m) ((eq m :b) m))
                     (mailbox-count)
                     (flush-mailbox)
                     (mailbox-count)))))

(with-test (:name (:process :messages :timeouts-and-flush))
  (with-exit (reason value) (spawn 'timeouts)
    (assert (eq reason :normal))
    (assert (equal value '(((nil nil) (nil nil) :timed-out (:b 2 2 0)))))))

(defun holder ()
  (receive (m) ((consp m) (setf (car m) :mutated) (send (cadr m) :done))))

(defun mutator (holder)
  (let ((message (list :original (self))))
    (send holder message)
    (receive-message :timeout 5)
    (car message)))

(with-test (:name (:process :messages :copied-between-heaps)
            :skipped-on (not :sb-process-heaps))
  (let* ((h (spawn 'holder))
         (m (spawn 'mutator :arguments (list h))))
    (with-exit (reason value) m
      (assert (eq reason :normal))
      (assert (equal value '(:original))))
    (join-process h :timeout 5)))

(with-test (:name (:process :messages :from-threads-and-names))
  (let ((p (spawn 'idler :name 'named-idler)))
    (assert-error (send 'no-such-name :x) unknown-name-error)
    (assert-error (spawn 'idler :name 'named-idler) name-taken-error)
    (send 'named-idler :go)
    (with-exit (reason value) p
      (assert (eq reason :normal)))
    ;; Sending to a process that has exited is not an error.
    (assert (eq (send p :late) :late))))

;;; --- Links and monitors ---

(defun watcher ()
  (multiple-value-bind (p ref) (spawn-monitor 'crasher)
    (receive (m :timeout 5)
      ((and (consp m) (eq (car m) :down) (eql (cadr m) ref) (eq (caddr m) p))
       (let ((reason (fourth m)))
         (list (car reason) (typep (second reason) 'simple-error)))))))

(defun late-monitor ()
  (let ((p (spawn 'return-42)))
    (await-process p)
    (let ((ref (monitor p)))
      (receive (m :timeout 5)
        ((and (consp m) (eq (car m) :down) (eql (cadr m) ref)) (fourth m))))))

(defun demonitor-flush ()
  (multiple-value-bind (p ref) (spawn-monitor 'return-42)
    (assert (eq (await-process p) :normal))
    (demonitor ref :flush t)
    (mailbox-count)))

(with-test (:name (:process :monitor))
  (with-exit (reason value) (spawn 'watcher)
    (assert (equal value '((:error t)))))
  (with-exit (reason value) (spawn 'late-monitor)
    (assert (equal value '(:noproc))))
  (with-exit (reason value) (spawn 'demonitor-flush)
    (assert (equal value '(0)))))

(defun linker ()
  (spawn-link 'crasher)
  (process-sleep 5)
  :survived)

(defun trapper ()
  (setf (trap-exits) t)
  (assert (trap-exits))
  (let ((c (spawn-link 'crasher)))
    (receive (m :timeout 5)
      ((and (consp m) (eq (car m) :exit) (eq (cadr m) c)) (car (caddr m)))
      (:timeout :timeout))))

(defun unlinker ()
  (let ((c (spawn-link 'idler)))
    (unlink c)
    (signal-exit c :kill)
    (process-sleep 0.1)
    :survived))

(defun link-dead ()
  (setf (trap-exits) t)
  (let ((p (spawn 'return-42)))
    (await-process p)
    (link p)
    (receive (m :timeout 5)
      ((and (consp m) (eq (car m) :exit) (eq (cadr m) p)) (caddr m)))))

(with-test (:name (:process :link))
  (with-exit (reason value) (spawn 'linker)
    (assert (and (consp reason) (eq (car reason) :error))))
  (with-exit (reason value) (spawn 'trapper)
    (assert (equal value '(:error))))
  (with-exit (reason value) (spawn 'unlinker)
    (assert (equal value '(:survived))))
  (with-exit (reason value) (spawn 'link-dead)
    (assert (equal value '(:noproc)))))

;;; --- Timers ---

(defun timed ()
  (let ((timer (send-after 10 (self) :never)))
    (assert (cancel-timer timer))
    (assert (not (cancel-timer timer))))
  (send-after 0.1 (self) :later)
  (let ((start (get-internal-real-time)))
    (multiple-value-bind (m found) (receive-message :timeout 2)
      (list m found
            (>= (- (get-internal-real-time) start)
                (* 0.09 internal-time-units-per-second))))))

(defun napper ()
  (send (self) :ignored-while-asleep)
  (let ((start (get-internal-real-time)))
    (process-sleep 0.1)
    (list (>= (- (get-internal-real-time) start)
              (* 0.09 internal-time-units-per-second))
          (mailbox-count))))

(with-test (:name (:process :timers))
  (with-exit (reason value) (spawn 'timed)
    (assert (equal value '((:later t t)))))
  (with-exit (reason value) (spawn 'napper)
    (assert (equal value '((t 1))))))

;;; --- Load ---

(defun pong ()
  (loop (receive (m)
          ((and (consp m) (eq (car m) :ping)) (send (cadr m) :pong))
          ((eq m :stop) (return)))))

(defun pinger (pong n)
  (dotimes (i n)
    (send pong (list :ping (self)))
    (receive (m) ((eq m :pong))))
  (send pong :stop)
  n)

(with-test (:name (:process :load :ping-pong))
  (let* ((before (length (list-processes)))
         (pongs (loop repeat 40 collect (spawn 'pong)))
         (pings (loop for p in pongs collect (spawn 'pinger :arguments (list p 500)))))
    (dolist (p pings)
      (with-exit (reason value) p
        (assert (equal value '(500)))))
    (dolist (p pongs) (join-process p :timeout 5))
    (assert (= (length (list-processes)) before))))

(defun allocator (n)
  (let ((keep nil))
    (dotimes (i n) (push (make-string 100 :initial-element #\x) keep)
      (when (zerop (mod i 1000)) (process-yield)))
    (length keep)))

(with-test (:name (:process :load :allocation-in-heaps)
            :skipped-on (not :sb-process-heaps))
  (let ((processes (loop repeat 20
                         collect (spawn 'allocator :arguments '(20000)
                                        :heap '(:gc-threshold 200000)))))
    (dolist (p processes)
      (with-exit (reason value) p
        (assert (equal value '(20000)))))
    (assert (null (sb-fiber:verify-all-heaps)))))

(with-test (:name (:process :scheduler :stop-with-live-processes))
  (let ((idlers (loop repeat 5 collect (spawn 'idler))))
    (assert (wait-until (lambda () (every (lambda (p) (eq (process-state p) :waiting)) idlers))))
    (assert (stop-scheduler))
    (assert (every (lambda (p) (not (process-alive-p p))) idlers))
    (assert (null (list-processes)))))
