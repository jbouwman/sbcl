# sb-fiber notes

These are some technical notes on implementation of fibers.
User-facing API documentation can be found in `sb-fiber.texinfo`.

## Code organization

| Role       | Files                               |
|------------|-------------------------------------|
| Lisp API   | `contrib/sb-fiber/fiber.lisp`       |
| Lisp <-> C | `contrib/sb-fiber/fiber-ffi.lisp`   |
| VOPs       | `contrib/sb-fiber/<arch>-vops.lisp` |
| Assembly   | `src/assembly/<arch>/tramps.lisp`   |
| Runtime    | `src/runtime/<arch>-fiber.{c, h}`   |
|            | `src/runtime/fiber.{c, h}`          |

C manages allocation of stacks, GC integration, binding stack swap,
pseudo-atomic entry and exit, and defines per-architecture context
structures to hold callee-saved registers that are preserved when a
fiber is switched.

Lisp manages argument validation, catch and unwind chain save and
install, re-signaling conditions captured by the trampoline, and
defines the VOP that implements register swap inline at the
`switch-fiber` call site.

## Stack layout

Fibers' control and binding stacks are mapped regions with guard pages
that follow the per-thread overflow layout.  On
`#-c-stack-is-control-stack` targets (e.g. arm64) the Lisp control
stack and the C stack are separate regions that both belong to a
fiber.

## Binding stack swap

A binding stack entry is `(value, tls-index)`, where value is the
TLS value held just before the binding was pushed.  `N` nested
bindings of the same symbol form a chain: entry `k` stores `V_{k-1}`,
and the live TLS slot holds `V_N`.

`swap_bindings_{forward,backward}` in `fiber.c`:

To suspend a fiber, walk the entries from newest to oldest, exchanging
each entry's saved value with the live TLS.  After the pass, entry `k`
holds `V_k` and TLS holds `V_0`. To resume, walk oldest to newest.

The `:tls-load-indirect` feature maintains an indirect cell per TLS
slot pointing at the live value; `exchange_binding_with_tls` maintains
it alongside the TLS exchange.

## Pseudo-atomic switch window

A thread is inconsistent during `switch-fiber`, so the swap is bracketed
by pseudo-atomic. The Lisp shim enters PA by calling the C function
`sb_fiber_switch_prep`, which stages BSP swap, control stack bounds
swap, state flip, and binding stack swap.

The `%swap-regs` VOP then does the register and SP swap, and
transfers control to the resuming fiber's stack.  The resuming side,
at the VOP's `RESUME` label, exits PA and checks to see if a signal
arrived during the window.

`sb_fiber_exit_pa` is the same exit path for the trampoline's
auto-return flow, which runs in C and can't use the VOP's exit.

## GC

`extra_thread_data->fiber_list` enumerates every registered fiber on
a thread: GC walks it.

Suspended fibers marked runnable or new have their saved SP range
`[ctx.sp .. stack_end)` and their callee-saved registers
conservatively pinned.  On arm64 the separate Lisp control stack
`[base, csp_save)` is pinned the same way; the region above
`csp_save` is dead and unscanned.

On arm64, a conservative scanner walks `[base, CSP_save)` on a
suspended fiber and pins anything pointer-shaped.

The words above a fiber's `CSP` are left over from returned frames.
A frame pushed later can expose them to the precise scan of the
running stack before it writes its slots, so they must not refer to
memory a collection has freed since they were written.  The
collectors zero the running stack above its `CSP`
(`scrub_thread_control_stack`); a suspended fiber's stack is zeroed
from `CSP` to `usable_end` by `sb_fiber_lisp_stack_resume`, before
the fiber runs again, if since it last ran

- a global collection has run,
- its own heap has been collected, or
- any local collection has run and the fiber has installed a heap
  other than its own.

A fiber resumed with no such collection in between pays nothing,
whatever depth it suspended at, and a new fiber's stack, fresh from
`mmap`, is not zeroed before its first run.  The extent of the words
to zero is not recorded: a fiber can call deeper than any point it
suspends at, and a frame's unwritten slots are zero, so neither the
suspend depth nor a run of zero words bounds them.

## Trampoline

When a fiber's entry function returns normally, control re-enters
`fiber_tramp_c`, which marks the fiber dead and switches to
`self->return_fiber` via `sb_fiber_switch_prep` + the assembly
`fiber_swap_context`.

The resuming fiber exits PA via its own VOP tail or
`sb_fiber_exit_pa`.

The Lisp wrapper for the resumed fiber is found via the
`current_fiber` slot of `struct thread`.  The Lisp shim stores `to`
there before the swap.  The resumed side stores itself again after the
swap because the C auto-return path does not write the slot.

## Current fiber

The current fiber is recorded twice: the Lisp wrapper in
`struct thread`'s `current_fiber`, which `current-fiber` reads, and
the `sb_fiber_ctx` in `extra_thread_data`'s `current_fiber`, which
the runtime uses to find the fiber a heap switch applies to
(`local_heap_switch_in_pa` keeps the running fiber's `active_heap`
up to date).  A switch writes both.  Registering a fiber writes
neither; `sb_fiber_set_current`, called by `make-main-fiber` and
`with-current-fiber`, writes the runtime's and copies the installed
heap into the fiber, since heap switches made while the fiber was
not current were not recorded on it.  Writing only the Lisp slot
leaves the runtime pointing at a different fiber, which is then
resumed with that fiber's heap.

Only a main fiber is installed without a switch, and only while the
thread has no current fiber.  A main fiber installed over a running
worker would have the worker's stack pointers saved into it by the
next switch.

## Image survival

A saved core's restart restores Lisp wrappers but not the C
`sb_fiber_ctx` structs they point at.  Thread structures are rebuilt
at startup, so every thread begins with no current fiber; user code
holding wrappers across `save-lisp-and-die` is on its own, like
`sb-thread`.
