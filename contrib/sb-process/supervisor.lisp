;;;; -*-  Lisp -*-
;;;;
;;;; Supervisors: processes that start children from specifications,
;;;; link to them, and restart them when they exit, in the manner of
;;;; OTP supervisors.  A supervisor is a generic server whose module is
;;;; the symbol SUPERVISOR.

(in-package :sb-process)

(define-condition supervisor-error (process-error) ())

(define-condition child-start-error (supervisor-error)
  ((id :initarg :id :reader child-start-error-id)
   (reason :initarg :reason :reader child-start-error-reason))
  (:report (lambda (c stream)
             (format stream "child ~S of ~S failed to start: ~S"
                     (child-start-error-id c) (process-error-process c)
                     (child-start-error-reason c)))))

(defstruct (child-spec (:constructor %make-child-spec) (:copier nil))
  (id nil)
  (function nil :type (or function symbol))
  (arguments nil :type list)
  ;; :PERMANENT restarts always, :TRANSIENT only after an abnormal exit,
  ;; :TEMPORARY never.
  (restart :permanent :type (member :permanent :transient :temporary))
  ;; Seconds to wait for the child to exit after a :SHUTDOWN signal
  ;; before killing it; or :BRUTAL-KILL.
  (shutdown 5 :type (or (real 0) (eql :brutal-kill)))
  (name nil :type symbol)
  (heap *default-process-heap*)
  (stack-size *default-process-stack-size* :type fixnum)
  (trap-exits nil :type boolean))

(defun make-child-spec (id function &key arguments (restart :permanent) (shutdown 5)
                                         name (heap *default-process-heap*)
                                         (stack-size *default-process-stack-size*)
                                         trap-exits)
  "Describe a child: ID identifies it within its supervisor; FUNCTION
and ARGUMENTS are as for SPAWN, as are NAME, HEAP, STACK-SIZE and
TRAP-EXITS.  RESTART is :PERMANENT, :TRANSIENT or :TEMPORARY; SHUTDOWN
is how long to wait for the child to exit when it is told to, or
:BRUTAL-KILL."
  (%make-child-spec :id id :function function :arguments arguments
                    :restart restart :shutdown shutdown :name name :heap heap
                    :stack-size stack-size :trap-exits trap-exits))

(defmethod print-object ((spec child-spec) stream)
  (print-unreadable-object (spec stream :type t)
    (format stream "~S ~(~A~)" (child-spec-id spec) (child-spec-restart spec))))

;;; One entry per child in the supervisor's state; lives in the
;;; supervisor's heap.
(defstruct (child (:copier nil))
  (spec nil :type child-spec)
  (process nil)
  (restarts 0 :type fixnum))

(defstruct (supervisor-state (:conc-name sup-) (:copier nil))
  (strategy :one-for-one :type (member :one-for-one :one-for-all :rest-for-one))
  (intensity 3 :type (integer 0))
  (period 5 :type (real 0))
  (children nil :type list)
  ;; Times of recent restarts, newest first.
  (restart-times nil :type list)
  (parent nil))

;;; --- Starting and stopping children ---

(defun start-child-process (spec)
  (let ((process (spawn (child-spec-function spec)
                        :arguments (child-spec-arguments spec)
                        :name (child-spec-name spec)
                        :link t
                        :heap (child-spec-heap spec)
                        :stack-size (child-spec-stack-size spec)
                        :trap-exits (child-spec-trap-exits spec))))
    process))

(defun exit-message-p (message process)
  (and (consp message) (eq (car message) :exit) (eq (cadr message) process)))

(defun wait-child-exit (process timeout)
  "Consume the :EXIT message of PROCESS.  Returns its reason and T, or
NIL, NIL after TIMEOUT."
  (receive (m :timeout timeout)
    ((exit-message-p m process) (values (caddr m) t))
    (:timeout (values nil nil))))

(defun stop-child-process (child)
  "Tell the child to exit and wait for it, killing it if it takes too
long.  Returns once its exit has been consumed (or given up on)."
  (let ((process (child-process child))
        (shutdown (child-spec-shutdown (child-spec child))))
    (when process
      (setf (child-process child) nil)
      (cond ((not (process-alive-p process))
             ;; It exited on its own; drop the exit message it left.
             (wait-child-exit process 0))
            (t
             (let ((done nil))
               (cond ((eq shutdown :brutal-kill)
                      (signal-exit process :kill)
                      (setf done (wait-child-exit process 5)))
                     (t
                      (signal-exit process :shutdown)
                      (setf done (wait-child-exit process shutdown))
                      (unless done
                        (signal-exit process :kill)
                        (setf done (wait-child-exit process 5)))))
               ;; A killed process that never yields cannot be reaped;
               ;; do not wait for it forever.
               (unless done (unlink process))))))
    nil))

(defun stop-children (children)
  "Stop CHILDREN, last started first."
  (dolist (child (reverse children))
    (stop-child-process child)))

;;; --- Restart policy ---

(defun restart-allowed-p (state)
  "Record a restart; NIL if the intensity has been exceeded."
  (let* ((now (now))
         (window (seconds->units (sup-period state)))
         (recent (remove-if (lambda (time) (> (- now time) window))
                            (sup-restart-times state))))
    (setf (sup-restart-times state) (cons now recent))
    (<= (length (sup-restart-times state)) (sup-intensity state))))

(defun abnormal-exit-p (reason)
  (not (or (eq reason :normal) (eq reason :shutdown)
           (and (consp reason) (eq (car reason) :shutdown)))))

(defun child-restart-wanted-p (child reason)
  (ecase (child-spec-restart (child-spec child))
    (:permanent t)
    (:transient (abnormal-exit-p reason))
    (:temporary nil)))

(defun restart-children (state children)
  "Start CHILDREN (in order).  Returns NIL if one failed to start."
  (declare (ignore state))
  (dolist (child children t)
    (handler-case (setf (child-process child) (start-child-process (child-spec child))
                        (child-restarts child) (1+ (child-restarts child)))
      (error (c)
        (report-process-error (self) c)
        (return nil)))))

(defun handle-child-exit (state child reason)
  "A linked child exited: restart according to the strategy."
  (setf (child-process child) nil)
  (cond ((not (child-restart-wanted-p child reason))
         (when (eq (child-spec-restart (child-spec child)) :temporary)
           (setf (sup-children state) (delete child (sup-children state))))
         (values :noreply state))
        ((not (restart-allowed-p state))
         (stop-children (sup-children state))
         (values :stop :shutdown state))
        (t
         (let ((restarted
                 (ecase (sup-strategy state)
                   (:one-for-one
                    (restart-children state (list child)))
                   (:one-for-all
                    (stop-children (remove child (sup-children state)))
                    (restart-children state (sup-children state)))
                   (:rest-for-one
                    (let ((rest (member child (sup-children state))))
                      (stop-children (cdr rest))
                      (restart-children state rest))))))
           (if restarted
               (values :noreply state)
               ;; A child that cannot be started counts as a crash of
               ;; the whole group: try again through the same policy.
               (handle-child-exit state child :start-failed))))))

;;; --- The supervisor as a server module ---

(defun find-child (state id)
  (find id (sup-children state) :key (lambda (child) (child-spec-id (child-spec child)))
                                :test #'equal))

(defmethod init ((module (eql 'supervisor)) arguments)
  (destructuring-bind (specs &key (strategy :one-for-one) (intensity 3) (period 5)
                             (parent (process-parent (self))))
      arguments
    (setf (trap-exits) t)
    (let ((state (make-supervisor-state
                  :strategy strategy :intensity intensity :period period
                  :parent parent
                  :children (mapcar (lambda (spec) (make-child :spec spec)) specs))))
      (dolist (child (sup-children state))
        (handler-case (setf (child-process child) (start-child-process (child-spec child)))
          (error (c)
            (stop-children (sup-children state))
            (return-from init (values :stop (list :error c))))))
      (values :ok state))))

(defmethod handle-info ((module (eql 'supervisor)) message state)
  (cond ((and (consp message) (eq (car message) :exit))
         (destructuring-bind (from reason) (cdr message)
           (let ((child (find from (sup-children state) :key #'child-process)))
             (cond (child (handle-child-exit state child reason))
                   ;; Our parent (or an exit signal from outside any
                   ;; process) takes us down with it.
                   ((and (or (null from) (eq from (sup-parent state)))
                         (not (eq reason :normal)))
                    (values :stop reason state))
                   (t (values :noreply state))))))
        (t (values :noreply state))))

(defmethod handle-call ((module (eql 'supervisor)) request from state)
  (declare (ignore from))
  (flet ((child-plist (child)
           (list :id (child-spec-id (child-spec child))
                 :process (child-process child)
                 :restarts (child-restarts child)
                 :spec (child-spec child))))
    (case (car request)
      (:which-children
       (values :reply (mapcar #'child-plist (sup-children state)) state))
      (:count-children
       (let ((children (sup-children state)))
         (values :reply (list :specs (length children)
                              :active (count-if (lambda (c) (and (child-process c)
                                                                 (process-alive-p (child-process c))))
                                                children))
                 state)))
      (:start-child
       (let ((spec (cadr request)))
         (if (find-child state (child-spec-id spec))
             (values :reply (list :error :already-present) state)
             (let ((child (make-child :spec spec)))
               (handler-case
                   (progn (setf (child-process child) (start-child-process spec))
                          (setf (sup-children state) (append (sup-children state) (list child)))
                          (values :reply (list :ok (child-process child)) state))
                 (error (c) (values :reply (list :error c) state)))))))
      (:terminate-child
       (let ((child (find-child state (cadr request))))
         (cond ((null child) (values :reply (list :error :not-found) state))
               (t (stop-child-process child)
                  (when (eq (child-spec-restart (child-spec child)) :temporary)
                    (setf (sup-children state) (delete child (sup-children state))))
                  (values :reply :ok state)))))
      (:restart-child
       (let ((child (find-child state (cadr request))))
         (cond ((null child) (values :reply (list :error :not-found) state))
               ((and (child-process child) (process-alive-p (child-process child)))
                (values :reply (list :error :running) state))
               (t (handler-case
                      (progn (setf (child-process child) (start-child-process (child-spec child)))
                             (values :reply (list :ok (child-process child)) state))
                    (error (c) (values :reply (list :error c) state)))))))
      (:delete-child
       (let ((child (find-child state (cadr request))))
         (cond ((null child) (values :reply (list :error :not-found) state))
               ((and (child-process child) (process-alive-p (child-process child)))
                (values :reply (list :error :running) state))
               (t (setf (sup-children state) (delete child (sup-children state)))
                  (values :reply :ok state)))))
      (t (values :reply (list :error :unknown-request) state)))))

(defmethod terminate ((module (eql 'supervisor)) reason state)
  (declare (ignore reason))
  (stop-children (sup-children state)))

;;; --- API ---

(defun start-supervisor (children &key (strategy :one-for-one) (intensity 3) (period 5)
                                       name link (heap *default-process-heap*))
  "Start a supervisor process for CHILDREN, a list of child specs (see
MAKE-CHILD-SPEC), starting them in order.  STRATEGY is :ONE-FOR-ONE,
:ONE-FOR-ALL or :REST-FOR-ONE; more than INTENSITY restarts within
PERIOD seconds make the supervisor stop its children and exit with
reason :SHUTDOWN.  NAME, LINK and HEAP are as for SPAWN.  Returns the
supervisor process."
  (dolist (spec children)
    (check-type spec child-spec))
  (start-server 'supervisor
                :arguments (list children :strategy strategy :intensity intensity
                                          :period period :parent *current-process*)
                :name name :link link :heap heap))

(defun run-supervisor (children &key (strategy :one-for-one) (intensity 3) (period 5))
  "Run a supervisor for CHILDREN in the calling process, as
START-SUPERVISOR would in a new one.  Returns only when the supervisor
exits.  This is the function to give a child spec whose child is itself
a supervisor."
  (dolist (spec children)
    (check-type spec child-spec))
  (run-server 'supervisor
              :arguments (list children :strategy strategy :intensity intensity
                                        :period period)))

(defun stop-supervisor (supervisor &key (reason :shutdown) (timeout 10))
  "Stop SUPERVISOR and its children; wait up to TIMEOUT seconds for it
to exit.  Returns T if it did."
  (let ((process (resolve-process supervisor)))
    (stop-server process :reason reason :timeout timeout)
    (if *current-process*
        (nth-value 1 (await-process process :timeout timeout))
        (nth-value 2 (join-process process :timeout timeout)))))

(defun which-children (supervisor)
  "A plist (:ID :PROCESS :RESTARTS :SPEC) per child of SUPERVISOR."
  (call supervisor '(:which-children)))

(defun count-children (supervisor)
  "A plist (:SPECS :ACTIVE) counting SUPERVISOR's children."
  (call supervisor '(:count-children)))

(defun supervisor-reply (reply)
  (cond ((eq reply :ok) t)
        ((and (consp reply) (eq (car reply) :ok)) (cadr reply))
        ((and (consp reply) (eq (car reply) :error))
         (if (typep (cadr reply) 'condition)
             (error (cadr reply))
             (error 'supervisor-error :process nil)))
        (t reply)))

(defun start-child (supervisor spec)
  "Add SPEC to SUPERVISOR's children and start it; returns the process."
  (supervisor-reply (call supervisor (list :start-child spec))))

(defun terminate-child (supervisor id)
  "Stop the child ID of SUPERVISOR, keeping its spec unless it is :TEMPORARY."
  (supervisor-reply (call supervisor (list :terminate-child id))))

(defun restart-child (supervisor id)
  "Start the stopped child ID of SUPERVISOR; returns the process."
  (supervisor-reply (call supervisor (list :restart-child id))))

(defun delete-child (supervisor id)
  "Remove the stopped child ID from SUPERVISOR."
  (supervisor-reply (call supervisor (list :delete-child id))))
