# sb-process notes

Implementation notes for the process layer.  The user-facing API is
documented in `sb-process.texinfo`.

## Code organization

| Role                                | File              |
|-------------------------------------|-------------------|
| Carriers, run queues, timers        | `scheduler.lisp`  |
| Processes, mailboxes, links, names  | `process.lisp`    |
| Generic server                      | `server.lisp`     |
| Supervisors                         | `supervisor.lisp` |

## Ownership

A process is a global descriptor (`process`) plus an `sb-fiber` fiber
that runs its function.  Descriptors, run queues, timers and the
registry are global objects and must never point into a process heap,
so every function that allocates on a process's behalf wraps the
allocation in `sb-fiber:without-heap`: pushing onto a run queue,
inserting a timer, registering a name, recording a link.  The store
barrier catches omissions as `process-heap-store-error`.

What lives in the process's own heap: the messages it has received,
the messages a selective receive has skipped (a list in the box bound
to `*saved-messages*` on the fiber's binding stack, which makes it a
root of the process), and the predicate closures `receive` builds.

Values that cross out of a process are copied: `spawn` globalizes the
arguments and the process copies them into its heap when it starts;
`finish-process` globalizes the exit reason and value (a condition that
cannot be copied is replaced by its printed form); `send-after`
globalizes the message when the timer is armed.

## Scheduling

Each carrier thread has a main fiber (`sb-fiber:with-fiber-thread`), a
run queue under its own lock, and a `*carrier*` binding.  A process
fiber is created on the carrier that first runs it (a fiber can only be
migrated once it has been suspended) and migrated with
`sb-fiber:fiber-migrate` whenever another carrier picks it up.
Idle carriers wait on one scheduler-wide condition variable, which
`enqueue-process` and `schedule-timer` notify, and steal from the other
run queues when woken.  Timers live in one binary heap under the
scheduler lock; any carrier fires the due ones.

Waiting uses a park token.  A process that has nothing to do sets its
state to `:waiting` under its lock, re-checks its mailbox (a message
enqueued between the check and the state change would otherwise be
missed), and yields to the carrier.  The carrier, once the fiber has
actually suspended, either sets `parked` or, if a wake arrived in
between (`wake-pending`), requeues the process at once.  `wake-process`
acts only on a `:waiting` process: it requeues a parked one and marks a
not-yet-parked one.

Lock order: a process lock may be held while taking a carrier lock,
and a carrier lock while taking the scheduler lock; two process locks
are taken in id order (`lock-pair`).  Timer callbacks run with no lock
held.

## Exit signals

`deliver-exit-signal` decides under the target's lock: a trapping
target gets an `(:exit from reason)` message; a `:normal` signal from
another process is dropped; anything else records `pending-exit`,
stages a `process-exit-signal` condition on the fiber with
`sb-fiber:interrupt-fiber` (delivered when the fiber is next resumed,
inside the process's own handlers) and wakes the process.  The message
operations, `process-yield` and `process-sleep` also check
`pending-exit`, so a running process notices a signal at its next such
point.  `process-exit-signal` is a `serious-condition` but not an
`error`, so `handler-case` for errors in process code does not swallow
it.

`process-main` establishes the handlers, publishes `started` (a
`:new` fiber is never interrupted; the wrapper checks `pending-exit`
itself), applies the function, and in an `unwind-protect` cleanup runs
`finish-process`: state `:exited`, name released, `:down` messages to
monitors, exit signals to links, and the exit semaphore signaled for
`join-process`.

## Servers and supervisors

`call` monitors the server, sends `(:call (caller . ref) request)` and
selectively receives `(:reply ref value)` or the monitor's `:down`.  A
thread that is not a process makes the call in a helper process and
joins it.  `start-server` returns after `init`: the new process
acknowledges the starter by message (a process) or semaphore (a
thread).

A supervisor is a server whose module is `supervisor`.  It traps exits
and links to its children, so a child's exit arrives as an `:exit`
message handled by `handle-info`; exits from its parent (the process
that spawned it) stop it, other non-child exits are ignored.  Stopping
a child sends `:shutdown`, waits `shutdown` seconds with a selective
receive for that child's `:exit`, then kills it; a child that never
reaches a yield point cannot be killed and is unlinked and abandoned.
Restart intensity is a list of recent restart times.

## Relation to epsilon

The design follows the process tree in the `epsilon` project's
`epsilon.supervisor`, with the changes that per-process heaps force:
messages are copied rather than shared, links deliver exit signals
rather than cancelling tokens (so `trap-exits` exists), receive is
selective (so servers reply to the caller's mailbox instead of a reply
channel), exit reasons are arbitrary copied values, monitor and exit
messages are never dropped, and the registry, links and monitors are
global structures updated only under `without-heap`.
