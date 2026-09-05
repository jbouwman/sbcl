;;;; -*-  Lisp -*-
;;;;
;;;; sb-process: an OTP-style process layer on sb-fiber.
;;;;
;;;; Processes are fibers scheduled cooperatively on a pool of carrier
;;;; threads, each with a mailbox that supports selective receive,
;;;; links, monitors, exit signals, a name registry, timers, a generic
;;;; server behaviour and supervision trees.  On builds with
;;;; :sb-process-heaps every process owns a heap of its own by default
;;;; and messages are copied between heaps, so that a process shares no
;;;; mutable state with any other and its memory is reclaimed in one
;;;; step when it exits.

(defpackage :sb-process
  (:use :cl)
  (:import-from :sb-thread
                #:make-mutex #:with-mutex #:make-waitqueue
                #:condition-wait #:condition-notify #:condition-broadcast
                #:make-semaphore #:wait-on-semaphore #:signal-semaphore)
  (:export
   ;; Scheduler
   #:start-scheduler
   #:stop-scheduler
   #:scheduler-running-p
   #:scheduler-carrier-count
   #:*default-carrier-count*
   #:*reductions-per-slice*
   ;; Processes
   #:process
   #:process-p
   #:spawn
   #:spawn-link
   #:spawn-monitor
   #:self
   #:current-process
   #:process-id
   #:process-name
   #:process-state
   #:process-alive-p
   #:process-exit-reason
   #:process-exit-value
   #:process-heap
   #:process-info
   #:list-processes
   #:*default-process-heap*
   #:*default-process-stack-size*
   #:*report-process-errors*
   ;; Messages
   #:send
   #:receive
   #:receive-message
   #:mailbox-count
   #:flush-mailbox
   ;; Control
   #:process-yield
   #:process-sleep
   #:exit-process
   #:signal-exit
   #:trap-exits
   #:link
   #:unlink
   #:monitor
   #:demonitor
   #:make-ref
   #:join-process
   #:await-process
   ;; Registry
   #:register
   #:unregister
   #:whereis
   #:registered
   ;; Timers
   #:send-after
   #:cancel-timer
   #:timer
   #:timer-p
   ;; Conditions
   #:process-error
   #:process-error-process
   #:no-current-process-error
   #:dead-process-error
   #:unknown-name-error
   #:unknown-name-error-name
   #:name-taken-error
   #:name-taken-error-name
   #:process-exit-signal
   #:process-exit-signal-from
   #:process-exit-signal-reason
   #:call-timeout
   #:call-failed
   #:call-failed-reason
   #:server-start-error
   #:server-start-error-reason
   ;; Generic server
   #:init
   #:handle-call
   #:handle-cast
   #:handle-info
   #:terminate
   #:start-server
   #:run-server
   #:stop-server
   #:call
   #:cast
   #:reply
   ;; Supervisor
   #:child-spec
   #:child-spec-p
   #:make-child-spec
   #:child-spec-id
   #:child-spec-function
   #:child-spec-arguments
   #:child-spec-restart
   #:child-spec-shutdown
   #:start-supervisor
   #:run-supervisor
   #:stop-supervisor
   #:which-children
   #:count-children
   #:start-child
   #:terminate-child
   #:restart-child
   #:delete-child
   #:supervisor-error
   #:child-start-error))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (sb-int:system-package-p (find-package "SB-PROCESS")) t))
