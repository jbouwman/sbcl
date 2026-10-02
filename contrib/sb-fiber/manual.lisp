(in-package :sb-manual)

(defsection @sb-fiber (:title "sb-fiber")
  "The `SB-FIBER` module, loadable by

      (require :sb-fiber)

  provides user-space coroutines, called fibers, with their own
  control and binding stacks. A fiber runs on one OS thread at a
  time, switches to another fiber only when it asks to, and can be
  moved between threads. The module is built when SBCL is configured
  with `--with-sb-fiber`, on x86-64 and arm64 without safepoints and
  not on win32; the feature is `:SB-FIBER`.

  SB-FIBER:SWITCH-FIBER is the one control transfer. It is the
  building block for schedulers, I/O integration and fiber-aware
  synchronization in user code; the module itself schedules nothing.

  A switch preserves the resumed fiber's control stack, binding
  stack, `*HANDLER-CLUSTERS*`, `*RESTART-CLUSTERS*`, catch and
  unwind-protect chains, callee-saved registers and, on x86-64, the
  SSE control word. A fiber's entry function runs with fresh
  `*HANDLER-CLUSTERS*` and `*RESTART-CLUSTERS*`. A condition that
  leaves the entry function unhandled is stored on the fiber and
  signalled again in the fiber that resumes it.

  Fibers do not survive SB-EXT:SAVE-LISP-AND-DIE: a saved core starts
  with no fibers, and references to fibers kept across a save are
  stale. A suspended fiber's C stack is scanned conservatively by the
  collector, on arm64 as well as x86-64.

      (require :sb-fiber)
      (use-package :sb-fiber)

      (with-fiber-thread ()
        (let ((child (make-fiber
                      (lambda ()
                        (format t \"hello from child~%\")
                        (yield-fiber)
                        (format t \"back again~%\")))))
          (resume-fiber child)
          (format t \"main resumed~%\")
          (resume-fiber child)))

  prints `hello from child`, `main resumed` and `back again`. A
  scheduler layers non-blocking I/O on this: a fiber that would block
  on a descriptor registers it with poll, epoll or kqueue and yields,
  and the scheduler's poll loop resumes the fiber when the descriptor
  is ready."
  (@sb-fiber-threads section)
  (@sb-fiber-lifecycle section)
  (@sb-fiber-switching section)
  (@sb-fiber-interrupts section)
  (@sb-fiber-introspection section)
  (@sb-fiber-conditions section))

(defsection @sb-fiber-threads (:title "Fibers and threads")
  "A thread takes part in fiber switching once it has a main fiber,
  which stands for the thread's own stacks. SB-FIBER:WITH-FIBER-THREAD
  makes one for the extent of its body; SB-FIBER:MAKE-MAIN-FIBER and
  SB-FIBER:WITH-CURRENT-FIBER are the explicit forms for a library
  that runs fibers on threads it does not own and keeps a main fiber
  registered between uses. Every fiber belongs to one thread, the only
  thread that may switch into it, until SB-FIBER:FIBER-MIGRATE hands
  it to another."
  (sb-fiber:with-fiber-thread macro)
  (sb-fiber:make-main-fiber function)
  (sb-fiber:with-current-fiber macro)
  (sb-fiber:current-fiber function)
  (sb-fiber:fiber-thread function)
  (sb-fiber:fiber-migrate function))

(defsection @sb-fiber-lifecycle (:title "Making and releasing fibers")
  "A fiber made by SB-FIBER:MAKE-FIBER is `:NEW` until first switched
  to, `:RUNNABLE` while suspended, `:RUNNING` while it runs and
  `:DEAD` once its entry function has returned. Registered fibers are
  released when their thread exits; SB-FIBER:RELEASE-FIBER reclaims
  one sooner."
  (sb-fiber:make-fiber function)
  (sb-fiber:with-fiber macro)
  (sb-fiber:release-fiber function)
  (sb-fiber:*default-fiber-stack-size* variable))

(defsection @sb-fiber-switching (:title "Switching")
  "SB-FIBER:SWITCH-FIBER suspends one fiber and resumes another,
  delivering values to the resumed fiber's switch, and makes the
  suspended fiber the resumed one's return fiber. SB-FIBER:RESUME-FIBER
  is the same with the current fiber as the one suspended, and
  SB-FIBER:YIELD-FIBER switches to the current fiber's return fiber
  without changing the return fiber of the target, so that a fiber
  resumed from several places returns to the latest. SB-FIBER:JOIN-FIBER
  resumes a fiber until it finishes."
  (sb-fiber:switch-fiber function)
  (sb-fiber:resume-fiber function)
  (sb-fiber:yield-fiber function)
  (sb-fiber:join-fiber function)
  (sb-fiber:fiber-return-fiber function)
  (sb-fiber:with-fiber-pinned macro)
  (sb-fiber:fiber-pinned-p function)
  (sb-fiber:fiber-pin-count function))

(defsection @sb-fiber-interrupts (:title "Interrupting a fiber")
  "Fibers are not preempted. SB-FIBER:INTERRUPT-FIBER stages a
  condition that the fiber signals in its own context when it is next
  resumed, or, for a fiber that has not run, before its entry function
  starts."
  (sb-fiber:interrupt-fiber function))

(defsection @sb-fiber-introspection (:title "Inspecting a fiber")
  (sb-fiber:fiber structure)
  (sb-fiber:fiberp function)
  (sb-fiber:fiber-name function)
  (sb-fiber:fiber-state function)
  (sb-fiber:fiber-alive-p function)
  (sb-fiber:fiber-main-p function)
  (sb-fiber:fiber-released-p function)
  (sb-fiber:fiber-control-stack-size function)
  (sb-fiber:fiber-control-stack-usage function))

(defsection @sb-fiber-conditions (:title "Conditions")
  "Every precondition error the module signals is a SB-FIBER:FIBER-ERROR
  carrying the fiber concerned, where there is one, so a scheduler can
  handle the base class or a subclass."
  (sb-fiber:fiber-error condition)
  (sb-fiber:fiber-error-fiber function)
  (sb-fiber:dead-fiber-error condition)
  (sb-fiber:pinned-fiber-error condition)
  (sb-fiber:pinned-fiber-error-depth function)
  (sb-fiber:fiber-thread-mismatch-error condition)
  (sb-fiber:fiber-thread-mismatch-error-role function)
  (sb-fiber:fiber-state-error condition)
  (sb-fiber:fiber-state-error-state function)
  (sb-fiber:fiber-state-error-expected function)
  (sb-fiber:no-current-fiber-error condition)
  (sb-fiber:no-current-fiber-error-operation function)
  (sb-fiber:current-fiber-error condition)
  (sb-fiber:current-fiber-error-operation function))
