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
install, `*current-fiber*`, re-signaling conditions captured by the
trampoline, and defines the VOP that implements register swap inline
at the `switch-fiber` call site.

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

`sb_fiber_lisp_stack_suspend` maintains a `dirty_high` per fiber: the
address such that `[dirty_high, usable_end)` is known clean.  On each
suspend:

- `CSP == dirty_high` (tight yield loop): no scrub.
- `CSP < dirty_high` (fiber returned to a shallower depth): scrub
  `[CSP, dirty_high)`.
- `CSP > dirty_high` (fiber grew above prior clean boundary):
  scrub all the way to `usable_end`.

Tight-yield fibers pay zero scrub cost after the first suspend.

## Trampoline

When a fiber's entry function returns normally, control re-enters
`fiber_tramp_c`, which marks the fiber dead and switches to
`self->return_fiber` via `sb_fiber_switch_prep` + the assembly
`fiber_swap_context`.

The resuming fiber exits PA via its own VOP tail or
`sb_fiber_exit_pa`.

The Lisp wrapper for the resumed fiber is found via
`*current-fiber*`. The Lisp shim sets it to `to` before the swap, and
TLS persists across the stack swap unchanged.

## Image survival

A saved core's restart restores Lisp wrappers but not the C
`sb_fiber_ctx` structs they point at.  An `*init-hooks*` callback
clears `*current-fiber*` on startup; user code holding wrappers across
`save-lisp-and-die` is on its own, like `sb-thread`.

## Process heaps

Available with `:sb-process-heaps` (requires `:mark-region-gc`).  A
process heap is a GC ownership domain: a set of 4 KiB blocks of
dynamic space tagged in a per-block owner table (`block_owner` in
`src/runtime/process-heap.c`, 0 = free or global).  A page holding any
process block is a "process page" (`ph_process_page`): the global
allocator never uses one, and its free blocks are available to every
heap, so many small heaps share a page.  Object addresses are ordinary
dynamic-space addresses; only the metadata differs.

| Role                          | Files                                   |
|-------------------------------|-----------------------------------------|
| Lisp API, copier, mailboxes   | `contrib/sb-fiber/heap.lisp`            |
| Fiber integration             | `contrib/sb-fiber/fiber{,-ffi}.lisp`    |
| Core primitives               | `src/code/process-heap.lisp`            |
| Descriptors, switching, mail  | `src/runtime/process-heap.{c,h}`        |
| Allocation, local GC, roots   | `src/runtime/process-heap-gc.inc`       |
| Collector hooks               | `src/runtime/mark-region.c`, `pmrgc.c`  |

### Allocation

The allocation fast path is untouched.  A thread's user TLABs
(`mixed_tlab`, `cons_tlab`) belong to whichever heap is installed on
it; `process_heap_switch` parks the current heap's free/limit pairs in
its descriptor and installs another's, and `sb_fiber_switch_prep` does
this on every fiber switch.  System TLABs are never switched, so code
compiled with `(declare (sb-c::tlab :system))` keeps allocating
globally.  The slow path (`process_heap_alloc_slow`) claims free lines
inside runs of blocks the heap already owns, then free blocks on a
process page, then a free page; heaps claim longer runs as they grow,
up to whole pages.  Objects of at least `LARGE_OBJECT_SIZE` take whole
pages.  A region never extends past its owner's run of blocks
(`try_allocate_small_after_region`, `mr_update_closed_region`), and
process pages carry no open-region flag since several heaps may have
regions open on one page.  The global allocator skips process pages.

### Collection

A local collection (`process_heap_collect_impl`) runs on the thread the
heap is installed on, inside `WITHOUT-GCING`.  Local collections on
different threads run at the same time: everything the global collector
keeps in globals lives in a per-thread `struct ph_local_gc` for the
local one (what to collect, a grey queue traced by that thread alone
rather than by the GC thread pool, and the weak-object lists, which
`pmrgc-impl.h` reaches through thread-local pointers that name the
global lists everywhere else).  What a local collection shares with the
world (the block table, page byte counts, the partial page lists, and
the collector's region for the smashed cells of weak tables) it touches
under the page table lock only.  It closes the heap's regions,
materializes allocation bits for fresh lines, scrubs the dead part of
the control stack, scans the thread's stacks, binding stack, TLS and
suspended fibers, then reuses mark-region's marking machinery with
`process_heap_target_owner` set to the heap: `mark()` treats anything
not owned by the target as a leaf, and `taggedptr_alivep_impl` treats
it as alive.  Sweeping walks the heap's runs of blocks; blocks that
end up empty go back to the page's free pool and pages nobody owns any
part of go back to the global free pool.  Materializing allocation
bits, scanning cards and raising generations are likewise bounded by
the heap's runs: a neighbouring heap may be allocating into its own
fresh lines on the same page at that very moment.  Weak pointers and weak hash tables in
the heap are culled; finalizers are refused for process objects.

Automatic collections are requested from the allocation slow path via
`pseudo_atomic_interrupted`; `interrupt_handle_pending` calls the
static fdefn `PROCESS-HEAP-COLLECT-PENDING`, mirroring `SUB-GC`.

Two environment variables help when debugging: `SBCL_PH_DEBUG` makes
`mark()` validate every object it marks and prints store violations,
and `SBCL_PH_VERIFY` runs `process_heap_verify` after every local
collection and dies on the first violation.

Heaps are generational using mark-region's own per-line generations
and card marks: allocation is generation 0; a minor collection sets
`generation_to_collect = 0`, so `mark()` treats old objects as leaves,
scans the marked cards of the heap's pages with the same
`scavenge_root_page` the global collector uses for its older
generations, sweeps only generation-0 lines and pages, and then raises
the survivors to generation 1.  A full collection raises everything to
generation 1 first and collects that, so it also needs a single marking
pass.  Either kind of collection ends by clearing the card marks of the
heap's blocks (sticky marks excepted): every survivor is old by then and
every edge out of the heap is in the summary, so a marked card
afterwards means exactly that its object was stored into since.  Full
collections happen on request, every `fullsweep_after`
minors, or when the old generation has doubled since the last one.  The
global card scan skips process pages: their card marks belong to the
heap's own collections, and it would otherwise reset them.

### Global collections

Process pages are neither marked nor swept by a global collection, but
whatever process objects refer to must stay alive.  Walking every
process object would make a global collection cost as much as all the
process heaps together, so each heap keeps an *outgoing global-root
summary*: while a local collection traces the heap, `mark()` records
every global object it meets as a leaf (`process_heap_note_outgoing`,
deduplicated with the global objects' own mark bits, which are unused
outside a global collection and cleared again at the end).  After a
local collection, every global object referred to from the heap's old
generation is in the summary, or the referring object sits on a card
marked since.  A global collection (`process_heaps_trace_roots`) marks
the summary and walks only the heap's young generation and the old
objects on marked cards, leaving the card marks alone; queued
fragments are entirely young.  A full local collection rebuilds the
summary, a minor one adds to it, so a stale entry retains a global
object until the heap's next full collection at worst.  Since the
objects walked include dead ones, edges from them to global memory are
treated as ambiguous (`find_object` must confirm an object start).
Compaction is disabled while any heap exists.  Parked regions are
closed in `process_heaps_before_global_gc`.

### Messages

`send-message` creates a fragment (a heap of kind FRAGMENT), installs
it, runs `copy-for-transfer`, seals it and enqueues it on the target's
mailbox.  `receive-message` retags the fragment's blocks as the
receiver's (`process_heap_adopt`): no payload copy.

A heap's descriptor is freed when the heap is released and its id may
then name a newer heap, so the entry points other threads use (send,
statistics, release) take an (id, epoch) handle and look the heap up
under the descriptor table lock, which a release holds while it takes
the heap out of the table; the owner's operations (install, collect,
receive, verify) use the descriptor directly.

### Store barrier

`emit-gengc-barrier` (both backends) emits, after the card mark, a
load of the thread's `process-heap-check` slot and a branch; when a
checked heap is installed, the object and each possibly-heap-allocated
value are pushed and the `process-heap-store-check` assembly routine
(patterned on `check-barrier`) calls `process_heap_check_store`, which
classifies the store against the ownership matrix and either records
it or signals `process-heap-store-error` through a static fdefn, with a
`continue` restart that performs the store.  The sites that pass an
unknown value to `emit-gengc-barrier` (`set-slot`, the indexed setters,
`set-fdefn-fun`) call `emit-process-heap-store-check` explicitly.
Stores the compiler elides the card mark for (fresh objects, immediate
or fixnum values, stack-allocated objects) are not checked; nor are
stores made from C.  Stores into stack-allocated objects (for example
the dynamic-extent string stream inside `format nil`) are never
violations.  Fragments and `without-heap` bodies run unchecked, and
`process-heap-store-error` suspends checking on the thread while the
condition is being signaled so that handlers do not recurse into it.

### System operations

Anything that builds global metadata on behalf of a process must run
with the global heap installed: class and generic function updates,
constructor generation, `compile`, `load` and `defstruct` are wrapped
in `sb-kernel::with-global-heap`; layouts and symbols are allocated
from the system TLAB.  PCL constructors are compiled with
`(sb-c::tlab :user)` so instances follow the caller's heap.
