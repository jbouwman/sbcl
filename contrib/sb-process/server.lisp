;;;; -*-  Lisp -*-
;;;;
;;;; A generic server: a process that handles synchronous calls,
;;;; asynchronous casts and other messages through methods specialized
;;;; on a "module" designator, as OTP's gen_server does through a
;;;; callback module.

(in-package :sb-process)

(defgeneric init (module arguments)
  (:documentation "Called in the new server process with the ARGUMENTS
given to START-SERVER.  Returns (VALUES :OK STATE) to run with STATE,
or (VALUES :STOP REASON) to exit."))

(defgeneric handle-call (module request from state)
  (:documentation "Handle a synchronous REQUEST made by CALL; FROM
identifies the caller for REPLY.  Returns (VALUES :REPLY REPLY STATE),
(VALUES :NOREPLY STATE) when the reply will be sent later with REPLY,
or (VALUES :STOP REASON REPLY STATE)."))

(defgeneric handle-cast (module message state)
  (:documentation "Handle a MESSAGE sent by CAST.  Returns (VALUES
:NOREPLY STATE) or (VALUES :STOP REASON STATE)."))

(defgeneric handle-info (module message state)
  (:documentation "Handle any other MESSAGE, such as a :DOWN or :EXIT
message.  Returns (VALUES :NOREPLY STATE) or (VALUES :STOP REASON STATE).
The default ignores the message."))

(defgeneric terminate (module reason state)
  (:documentation "Called before the server exits with REASON.  The
default does nothing."))

(defmethod handle-call (module request from state)
  (declare (ignore from))
  (error "~S does not handle the call ~S" module request))

(defmethod handle-cast (module message state)
  (error "~S does not handle the cast ~S" module message))

(defmethod handle-info (module message state)
  (declare (ignore module message))
  (values :noreply state))

(defmethod terminate (module reason state)
  (declare (ignore module reason state))
  nil)

(define-condition server-start-error (process-error)
  ((reason :initarg :reason :reader server-start-error-reason))
  (:report (lambda (c stream)
             (format stream "~S did not start: ~S"
                     (process-error-process c) (server-start-error-reason c)))))

(defun start-server (module &rest keys &key arguments name link monitor heap
                                        stack-size binding-stack-size (timeout 5))
  "Spawn a process running a generic server whose behaviour is given by
methods on MODULE, and return it once INIT has run with ARGUMENTS.  The
other keyword arguments are those of SPAWN.  Signals SERVER-START-ERROR
if INIT returns :STOP or fails, and CALL-TIMEOUT if it takes longer
than TIMEOUT seconds."
  (declare (ignore arguments name link monitor heap stack-size binding-stack-size))
  (let* ((keys (copy-list keys))
         (arguments (getf keys :arguments))
         (starter *current-process*)
         (ref (make-ref))
         ;; A thread that is not a process waits on a semaphore for the
         ;; outcome, left in a global box.
         (box (unless starter (without-heap (list nil))))
         (semaphore (unless starter (without-heap (make-semaphore :name "server start"))))
         (ack (without-heap
                (if starter
                    (list :process starter ref)
                    (list :semaphore semaphore box)))))
    (remf keys :arguments)
    (remf keys :timeout)
    (let ((process (apply #'spawn #'server-main
                          :arguments (list module arguments ack) keys)))
      (flet ((outcome (outcome)
               (cond ((eq (car outcome) :ok) process)
                     (t (error 'server-start-error :process process
                                                   :reason (cadr outcome))))))
        (cond (starter
               (let ((mref (monitor process)))
                 (receive (m :timeout timeout)
                   ((and (consp m) (eq (car m) :server-started) (eql (cadr m) ref))
                    (demonitor mref)
                    (outcome (caddr m)))
                   ((and (consp m) (eq (car m) :down) (eql (cadr m) mref))
                    (outcome (list :error (cadddr m))))
                   (:timeout
                    (demonitor mref :flush t)
                    (error 'call-timeout :process process)))))
              (t
               (unless (wait-on-semaphore semaphore :timeout timeout)
                 (error 'call-timeout :process process))
               (outcome (car box))))))))

(defun acknowledge-start (ack outcome)
  (let ((outcome (without-heap (sb-fiber:globalize outcome))))
    (ecase (car ack)
      (:process
       (%send (cadr ack) (without-heap (list :server-started (caddr ack) outcome))))
      (:semaphore
       (without-heap (setf (car (caddr ack)) outcome))
       (signal-semaphore (cadr ack))))))

(defun server-main (module arguments ack)
  (multiple-value-bind (status state)
      (handler-case (init module arguments)
        (serious-condition (c)
          (acknowledge-start ack (list :error (list :error c)))
          (error c)))
    (ecase status
      (:ok
       (acknowledge-start ack (list :ok))
       (server-loop module state))
      (:stop
       (acknowledge-start ack (list :error state))
       (exit-process state)))))

(defun run-server (module &key arguments)
  "Run a generic server for MODULE in the calling process; returns
only when the server stops, by exiting the process."
  (multiple-value-bind (status state) (init module arguments)
    (ecase status
      (:ok (server-loop module state))
      (:stop (exit-process state)))))

(defun server-stop (module reason state)
  (terminate module reason state)
  (exit-process reason))

(defun server-loop (module state)
  (loop
    (receive (message)
      ((and (consp message) (eq (car message) :call))
       (destructuring-bind (from request) (cdr message)
         (if (and (consp request) (eq (car request) :%stop))
             (progn (reply from :ok)
                    (server-stop module (cdr request) state))
             (multiple-value-bind (status a b c) (handle-call module request from state)
               (ecase status
                 (:reply (reply from a) (setf state b))
                 (:noreply (setf state a))
                 (:stop (reply from b) (server-stop module a c)))))))
      ((and (consp message) (eq (car message) :cast))
       (multiple-value-bind (status a b) (handle-cast module (cadr message) state)
         (ecase status
           (:noreply (setf state a))
           (:stop (server-stop module a b)))))
      (t
       (multiple-value-bind (status a b) (handle-info module message state)
         (ecase status
           (:noreply (setf state a))
           (:stop (server-stop module a b))))))))

(defun reply (from value)
  "Send VALUE as the reply to the call identified by FROM."
  (%send (car from) (list :reply (cdr from) value))
  value)

(defun cast (server message)
  "Send MESSAGE to SERVER for HANDLE-CAST."
  (send server (list :cast message))
  t)

(defun call (server request &key (timeout 5))
  "Send REQUEST to SERVER for HANDLE-CALL and return its reply.
Signals CALL-TIMEOUT after TIMEOUT seconds (NIL waits forever) and
CALL-FAILED if the server exits meanwhile.  May be called from any
thread; a thread that is not a process waits in a helper process."
  (let ((process (resolve-process server)))
    (if *current-process*
        (let ((ref (monitor process)))
          (send process (list :call (cons (self) ref) request))
          (receive (message :timeout timeout)
            ((and (consp message) (eq (car message) :reply) (eql (cadr message) ref))
             (demonitor ref)
             (caddr message))
            ((and (consp message) (eq (car message) :down) (eql (cadr message) ref))
             (error 'call-failed :process process :reason (cadddr message)))
            (:timeout
             (demonitor ref :flush t)
             (error 'call-timeout :process process))))
        (call-from-thread process request timeout))))

(defun call-helper (server request timeout)
  (call server request :timeout timeout))

(defun call-from-thread (process request timeout)
  (let ((helper (spawn #'call-helper :arguments (list process request timeout)
                                     :heap nil)))
    (multiple-value-bind (reason value done)
        (join-process helper :timeout (and timeout (+ timeout 1)))
      (cond ((not done)
             (deliver-exit-signal helper nil :kill)
             (error 'call-timeout :process process))
            ((eq reason :normal) (first value))
            ((and (consp reason) (eq (car reason) :error)
                  (typep (second reason) 'condition))
             (error (second reason)))
            (t (error 'call-failed :process process :reason reason))))))

(defun stop-server (server &key (reason :normal) (timeout 5))
  "Ask SERVER to exit with REASON, after calling TERMINATE, and wait for
its reply."
  (call server (cons :%stop reason) :timeout timeout))
