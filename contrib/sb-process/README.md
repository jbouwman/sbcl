# sb-process notes

Implementation notes for the process layer.  The user-facing API is
documented in `sb-process.texinfo`.

## Code organization

| Role                                | File              |
|-------------------------------------|-------------------|
| Carriers, run queues, timers        | `scheduler.lisp`  |
| Processes, mailboxes, links, names  | `process.lisp`    |

## Ownership

A process is a global descriptor plus an `sb-fiber` fiber
that runs its function.  Descriptors, run queues, timers and the
registry are global objects and must never point into a process heap,
so every function that allocates on a process's behalf wraps the
allocation in `sb-fiber:without-heap`: pushing onto a run queue,
inserting a timer, registering a name, recording a link.  The store
barrier catches omissions as `process-heap-store-error`.

The process's own heap contains the messages it has received, the
messages a selective receive has skipped, and the predicate closures
`receive` builds.

Values that cross out of a process are copied: `spawn` globalizes the
arguments and the process copies them into its heap when it starts;
`finish-process` globalizes the exit reason and value; `send-after`
globalizes the message when the timer is armed.

## Scheduling

Each carrier thread has a main fiber, a run queue under its own lock,
and a `*carrier*` binding.  A process fiber is created on the carrier
that first runs it and migrated with `sb-fiber:fiber-migrate` whenever
another carrier picks it up.  Idle carriers wait on one scheduler-wide
condition variable, which `enqueue-process` and `schedule-timer`
notify, and steal from the other run queues when woken.  Timers live
in one binary heap under the scheduler lock; any carrier fires the due
ones.

Waiting uses a park token.  A process that has nothing to do sets its
state to `:waiting` under its lock, re-checks its mailbox, and yields
to the carrier.  The carrier, once the fiber has actually suspended,
either sets `parked` or, if a wake arrived in between, requeues the
process at once.  `wake-process` acts only on a `:waiting` process: it
requeues a parked one and marks a not-yet-parked one.

Lock order: a process lock may be held while taking a carrier lock,
and a carrier lock while taking the scheduler lock; two process locks
are taken in id order.  Timer callbacks run with no lock held.

## Exit signals

`deliver-exit-signal` decides under the target's lock: a trapping
target gets an `(:exit from reason)` message; a `:normal` signal from
another process is dropped; anything else records `pending-exit`,
stages a `process-exit-signal` condition on the fiber with
`sb-fiber:interrupt-fiber` and wakes the process.  The message
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
