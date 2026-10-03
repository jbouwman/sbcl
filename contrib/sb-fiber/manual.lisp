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
  (@sb-fiber-conditions section)
  #+sb-local-heaps (@sb-fiber-heaps section))

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

#+sb-local-heaps
(defsection @sb-fiber-heaps (:title "Local heaps")
  "On builds with the `:SB-LOCAL-HEAPS` feature, which requires the
  mark-region collector, a fiber can own a local heap: a set of 4 KiB
  blocks of dynamic space that belongs to that fiber alone, in the
  manner of an Erlang/OTP process's heap. A heap that holds only a few
  objects costs two blocks, and several heaps may share a page.
  Everything the fiber allocates while it runs goes to its heap. The
  heap is collected on its own, without stopping other threads or
  fibers, and is returned to the global free pool in one step when the
  fiber is released. The global collector does not compact while any
  local heap exists.

  The invariant that makes this possible is ownership: an object in a
  local heap may refer to objects in the same heap and to global
  objects, but no global object and no other heap may refer into it.
  Objects therefore move between heaps only by copying
  (SB-FIBER:SEND-MESSAGE and SB-FIBER:RECEIVE-MESSAGE, or
  SB-FIBER:COPY-FOR-TRANSFER), and values crossing a fiber boundary
  (SB-FIBER:SWITCH-FIBER arguments and results, entry-function
  results, escaping conditions) are copied out of a local heap
  automatically. The invariant is enforced by a store barrier: while a
  heap is installed, every compiled pointer store checks that it does
  not make a global object or another heap refer into the installed
  heap, and signals SB-FIBER:HEAP-STORE-ERROR (continuable) when it
  would. The barrier does not see stores made from C or into objects
  the compiler knows to be freshly allocated by system code, so
  SB-FIBER:VERIFY-HEAP, SB-FIBER:VERIFY-ALL-HEAPS and
  SB-FIBER:HEAP-REFERENCE-CHECKING remain available to check the
  invariant after the fact. An unchecked violation is a bug in the
  program, as it would be with `SB-VM:WITH-ARENA`: the heap's collector
  will not see the offending reference, and the referenced object may
  be freed.

  Heaps can also be used without fibers, through SB-FIBER:WITH-HEAP."
  (@sb-fiber-heap-lifecycle section)
  (@sb-fiber-heap-barrier section)
  (@sb-fiber-heap-collection section)
  (@sb-fiber-heap-messages section)
  (@sb-fiber-heap-verification section)
  (@sb-fiber-heap-layers section))

#+sb-local-heaps
(defsection @sb-fiber-heap-lifecycle (:title "Making, installing and releasing heaps")
  "A heap is installed on one thread at a time, as the allocation
  target of that thread. SB-FIBER:WITH-HEAP installs one for the
  extent of its body; a fiber made with the `:HEAP` argument of
  SB-FIBER:MAKE-FIBER has its heap installed whenever it runs, and
  released with it. SB-FIBER:WITHOUT-HEAP installs the global heap on
  the installed heap's behalf, for work on global state."
  (sb-fiber:heap structure)
  (sb-fiber:heap-p function)
  (sb-fiber:make-heap function)
  (sb-fiber:*default-heap-gc-threshold* variable)
  (sb-fiber:release-heap function)
  (sb-fiber:with-heap macro)
  (sb-fiber:without-heap macro)
  (sb-fiber:current-heap function)
  (sb-fiber:object-heap function)
  (sb-fiber:fiber-heap function)
  (sb-fiber:heap-fiber function)
  (sb-fiber:heap-name function)
  (sb-fiber:heap-id function)
  (sb-fiber:heap-alive-p function)
  (sb-fiber:heap-released-p function)
  (sb-fiber:heap-error condition)
  (sb-fiber:dead-heap-error condition)
  (sb-fiber:heap-in-use-error condition)
  (sb-fiber:heap-not-current-error condition)
  (sb-fiber:no-current-heap-error condition))

#+sb-local-heaps
(defsection @sb-fiber-heap-barrier (:title "The store barrier")
  "While a heap is installed, every compiled pointer store checks
  that it does not make a global object or another heap refer into the
  installed heap. What the barrier does with a violation is the heap's
  checking mode, set by SB-FIBER:MAKE-HEAP: `:ERROR` signals
  SB-FIBER:HEAP-STORE-ERROR, `:RECORD` only records the violation (see
  SB-FIBER:HEAP-VIOLATIONS), `NIL` does not check at all. Only stores of
  pointers are checked; storing a fixnum, character or other immediate
  cannot create a reference. A strict heap additionally refuses any
  pointer store into a global object, so that the process can mutate
  nothing but its own data; operations that must touch global state
  are wrapped in SB-FIBER:WITHOUT-HEAP, which turns the strict rule off
  for its extent while the barrier still refuses an escape there.
  Strict mode applies to whatever runs while the heap is installed,
  including error handlers established outside the process: a handler
  that records the error in global state, or reads a slot of the
  condition, must do so inside SB-FIBER:WITHOUT-HEAP.

  A store whose value the compiler knows at the store to be global is
  not checked in either mode, since it cannot make anything refer into
  a heap. That covers a literal constant of the compiled code (a
  keyword or a quoted list, for instance) and a value whose type admits
  only fixnums, characters, single-floats, `NIL`, `T` and symbols of the
  initial core. Under strictness, such a store into a global object is
  therefore not a violation. The exemption depends on what the compiler
  can prove at the store: the same keyword arriving as a function
  argument is checked, and refused.

  Rules for code running in a local heap:

  - Data structures reachable from global objects must not hold
    locally-owned objects. Storing into a global hash table, a global
    symbol's value or property list, a special variable's global value,
    a closure created outside the process, or a variable captured by
    such a closure, is a violation; the store barrier signals it.
    Thread-local values (special bindings) are roots of the process and
    are fine.
  - Class and generic function definition, method addition,
    compilation and loading run with the global heap installed
    automatically, so the metadata they create is global. Other
    operations that build global state (interning into packages,
    creating threads, `FINALIZE`, pathname interning of process
    strings) must be wrapped in SB-FIBER:WITHOUT-HEAP or avoided.
  - `MAKE-INSTANCE`, structure constructors and ordinary allocation
    follow the installed heap. Layouts and symbols are always global.
  - A fiber's entry function must be global: a symbol, or a function
    not owned by a local heap.
  - Do not create threads or use `SAVE-LISP-AND-DIE` while local heaps
    exist; the runtime refuses to save a core with live heaps."
  (sb-fiber:heap-check-stores function)
  (sb-fiber:heap-strict-p function)
  (sb-fiber:heap-store-error condition)
  (sb-fiber:heap-store-error-object function)
  (sb-fiber:heap-store-error-value function)
  (sb-fiber:heap-store-error-kind function))

#+sb-local-heaps
(defsection @sb-fiber-heap-collection (:title "Collecting a heap")
  "A heap has two generations. A minor collection traces only the
  young generation, everything allocated since the previous
  collection, using the old generation's marked cards as additional
  roots, and promotes the survivors; a full collection, asked for or
  called for by the heap's policy, promotes everything and collects the
  old generation as well. Only the heap's objects are traced and swept;
  other threads keep running, and their own local collections proceed
  at the same time. The roots are the owning thread's control stack,
  binding stack and thread-local values, plus the stacks of suspended
  fibers on the same thread. Weak pointers and weak hash tables in the
  heap are honored; finalizers are not supported for objects in a local
  heap.

  A global collection does not walk a heap's old generation. Each local
  collection records the global objects the heap refers to in an
  outgoing root summary; a global collection marks that summary and
  walks only the heap's young generation and the old objects modified
  since the last local collection, so its cost does not grow with the
  size of the local heaps. A full local collection rebuilds the summary;
  until then a stale entry can keep a global object alive.

  The heap's size is bounded by its hard limit, reached which
  allocation signals SB-FIBER:LOCAL-HEAP-EXHAUSTED-ERROR, a
  `STORAGE-CONDITION`, with the global heap installed; and an
  allocation trap signals SB-FIBER:HEAP-ALLOCATION-TRAP once the bytes
  ever claimed pass a threshold, through
  SB-FIBER:*HEAP-ALLOCATION-TRAP-FUNCTION* when it is set.

  The statistics are bytes of dynamic space claimed (block granular),
  bytes retained by the last collection, bytes claimed since it, every
  byte ever claimed, pages on which the heap owns a block, blocks
  owned, global objects in the heap's outgoing root summary, objects of
  the heap walked as roots by the most recent global collection, the
  number of collections, microseconds spent collecting, bytes in the
  old generation, and the numbers of minor and full collections."
  (sb-fiber:heap-gc function)
  (sb-fiber:heap-fullsweep-after function)
  (sb-fiber:heap-hard-limit function)
  (sb-fiber:local-heap-exhausted-error condition)
  (sb-fiber:arm-heap-allocation-trap function)
  (sb-fiber:disarm-heap-allocation-trap function)
  (sb-fiber:heap-allocation-trap-threshold function)
  (sb-fiber:heap-allocation-trap condition)
  (sb-fiber:heap-allocation-trap-heap function)
  (sb-fiber:heap-allocation-trap-claimed function)
  (sb-fiber:heap-allocation-trap-trap function)
  (sb-fiber:*heap-allocation-trap-function* variable)
  (sb-fiber:heap-bytes-allocated function)
  (sb-fiber:heap-bytes-live function)
  (sb-fiber:heap-bytes-since-gc function)
  (sb-fiber:heap-bytes-claimed function)
  (sb-fiber:heap-page-count function)
  (sb-fiber:heap-block-count function)
  (sb-fiber:heap-outgoing-count function)
  (sb-fiber:heap-global-root-count function)
  (sb-fiber:heap-gc-count function)
  (sb-fiber:heap-gc-run-time function)
  (sb-fiber:heap-bytes-old function)
  (sb-fiber:heap-minor-gc-count function)
  (sb-fiber:heap-major-gc-count function)
  (sb-fiber:local-gc-concurrency-peak function))

#+sb-local-heaps
(defsection @sb-fiber-heap-messages (:title "Messages and transfer")
  "Objects cross between heaps by copying. SB-FIBER:SEND-MESSAGE
  copies an object into a private fragment that the receiving heap
  adopts, without a second copy, when it calls
  SB-FIBER:RECEIVE-MESSAGE; SB-FIBER:COPY-FOR-TRANSFER is the copy
  itself, into the installed heap; SB-FIBER:GLOBALIZE copies into the
  global heap. A shared binary is a specialized simple vector in the
  global heap, which every heap may refer to: it travels in messages
  without being copied, a global collection reclaims it once no heap
  refers to it, and it is to be treated as immutable once sent."
  (sb-fiber:send-message function)
  (sb-fiber:receive-message function)
  (sb-fiber:heap-mailbox-count function)
  (sb-fiber:heap-mailbox-bytes function)
  (sb-fiber:copy-for-transfer function)
  (sb-fiber:globalize function)
  (sb-fiber:make-shared-binary function)
  (sb-fiber:shared-binary-p function)
  (sb-fiber:untransferable-object condition)
  (sb-fiber:untransferable-object-object function)
  (sb-fiber:cross-heap-reference condition)
  (sb-fiber:cross-heap-reference-object function)
  (sb-fiber:heap-fiber-escape condition)
  (sb-fiber:heap-fiber-escape-fiber function)
  (sb-fiber:heap-fiber-escape-condition-type function)
  (sb-fiber:heap-fiber-escape-message function))

#+sb-local-heaps
(defsection @sb-fiber-heap-verification (:title "Verification")
  "The verifier and the collectors check the ownership invariant
  after the fact, for what the barrier does not see. Each violation is
  a list `(SOURCE-OBJECT SLOT-ADDRESS TARGET-ADDRESS ORIGIN STORE-PC)`:
  ORIGIN is `:COLLECTION` for a pointer a collection or verification
  found, `:RECORDED-STORE` for a store the barrier recorded and let
  proceed, and `:SIGNALED-STORE` for a store it signaled
  SB-FIBER:HEAP-STORE-ERROR for, which left the object unchanged unless
  a handler continued the store. For a store, SLOT-ADDRESS is 0 and
  STORE-PC is the return address into the code that made it, which
  SB-FIBER:STORE-SITE names; for a collection STORE-PC is `NIL`."
  (sb-fiber:verify-heap function)
  (sb-fiber:verify-all-heaps function)
  (sb-fiber:heap-reference-checking function)
  (sb-fiber:heap-violations function)
  (sb-fiber:reset-heap-violations function)
  (sb-fiber:take-heap-violations function)
  (sb-fiber:store-site function))

#+sb-local-heaps
(defsection @sb-fiber-heap-layers (:title "Building a process layer")
  "`SB-FIBER` stops at the primitives. A process layer in the manner
  of Erlang/OTP, with carriers, mailboxes with selective receive,
  links, monitors, exit signals, a registry and timers, is a library
  outside SBCL, and everything it needs is exported here:
  SB-FIBER:MAKE-FIBER with `:HEAP` for a process that owns its memory,
  SB-FIBER:SEND-MESSAGE and SB-FIBER:RECEIVE-MESSAGE for a copy in and
  an adoption out, SB-FIBER:HEAP-MAILBOX-COUNT as a wake condition a
  carrier can read from outside the heap, SB-FIBER:INTERRUPT-FIBER for
  an exit signal delivered at the next resume, SB-FIBER:WITHOUT-HEAP
  for bookkeeping the layer keeps in global structures, and
  SB-FIBER:RELEASE-FIBER to return a process's memory in one step. The
  test file `tests/fiber-carriers.impure.lisp` is the least such layer,
  a pool of carrier threads, and exercises those primitives with the
  sender and the receiver on different threads.")
