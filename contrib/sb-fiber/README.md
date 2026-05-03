# sb-fiber notes

C owns mmap'd stacks, binding-stack swap, pseudo-atomic entry/exit, GC
registration, and a per-thread freelist.

Lisp validates switch arguments, saves and pre-installs catch and
unwind-protect blocks, owns `*current-fiber*`, and re-signals
conditions captured by a trampoline.
     
Inline VOP wmits the register/SP swap inline at the FIBER-SWITCH call
site, avoiding the alien-call round-trip into `fiber_swap_context`.
The asm stub is still used by `fiber_trampoline_c`'s auto-return path,
which runs outside Lisp.

## Stack layout

Each fiber's control stack is a single mmap with guard pages that
follows the per-thread overflow layout in `validate.h`.

The binding stack is a separate mmap: usable region followed by a
single PROT_NONE guard page so overflow on `BIND` traps at the
faulting store rather than corrupting adjacent memory.

## Binding-stack swap

A binding-stack entry is `(tls-index, saved-value)` -- the TLS value
the symbol held immediately before this binding was pushed.  N
bindings of the same symbol thread a chain: entry k stores V_{k-1};
the live TLS slot holds V_N.

To suspend a fiber, swap each entry's saved value with the live TLS,
top-down.  After the pass, entry k holds V_k and TLS = V_0. To resume
a fiber, swap bottom-up: see `fiber.c`.

## Pseudo-atomic switch window

A switch transiently leaves the thread in a state the GC must not
observe: BSP and `th->control_stack_*` belong to one fiber while
register state still belongs to the other, and `state` words are
mid-flip.  Bracketing the swap in pseudo-atomic defers signals
(including STOP_FOR_GC) for the duration, so the GC's STW only ever
sees a coherent before-or-after.

The suspending side enters PA in `sb_fiber_switch_prep` (alien C):
it sets `th->pseudo_atomic_bits = (uword_t)th`, swaps BSP, swaps
the thread-slot Lisp-stack bounds, flips the state words, and runs
the binding-stack swap.  The `%fiber-register-swap` VOP then performs
the register/SP swap; it does *not* exit PA -- control transfers
to the resuming fiber's stack while still inside the region.  The
resuming side, at the VOP's `RESUME` tail, exits PA:
`__sync_xor_and_fetch(pa_bits, th)` clears the high bits in one
atomic; if bit 0 was set by a signal that arrived during the window,
`ud2` traps to `do_pending_interrupt`.

`sb_fiber_exit_pa` in `fiber.c` is the same exit path for
`fiber_trampoline_c`'s auto-return-to-`return_fiber` flow, which
runs in C and so cannot use the VOP's exit.

## GC

`extra_thread_data->fiber_list` enumerates every registered fiber on
a thread; the GC walks it under stop-the-world.

Fiber registration uses an atomic-release store to publish
`fiber->next` before the new head pointer is visible to a concurrent
GC reader.

## Freelist

`sb_fiber_destroy` parks default-sized child fibers on the owning
thread's `fiber_freelist` instead of `munmap`'ing them.

`sb_fiber_pool_drain` is called from `free_thread_struct` so a thread
exit releases its parked fibers.

## Trampoline

When a fiber's entry function returns normally, control re-enters
`fiber_trampoline_c`, which marks the fiber `FIBER_DEAD` and
auto-switches to `self->return_fiber`.  The auto-switch enters PA via
`sb_fiber_switch_prep` and uses the asm `fiber_swap_context` for the
register transfer.  The resuming fiber exits PA via its own VOP-tail
or `sb_fiber_exit_pa`.
