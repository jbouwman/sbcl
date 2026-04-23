#include "fiber.h"
#include "os.h"
#include "globals.h"
#include "thread.h"
#include "validate.h"
#include "genesis/symbol.h"
#include "lispobj.h"
#include <sys/mman.h>
#include <string.h>
#include <assert.h>
#include <stdint.h>

#ifndef MAP_STACK
/* Darwin and some BSDs do not define MAP_STACK.  It is advisory on
 * Linux and a no-op elsewhere; fall back to plain anonymous mapping. */
#define MAP_STACK 0
#endif

static size_t align_up(size_t v, size_t align) {
    return (v + align - 1) & ~(align - 1);
}

/* --- Per-thread idle-fiber freelist ---
 *
 * sb_fiber_destroy parks a default-sized idle fiber on the owning
 * thread's freelist instead of returning its stacks to the kernel, so
 * that back-to-back make/destroy pairs avoid mmap/munmap traffic.
 *
 * Invariants:
 *   - A pooled fiber is NOT on fiber_list.  sb_fiber_destroy
 *     unregisters first; sb_fiber_create is followed by Lisp-side
 *     %fiber-register for a revived fiber.
 *   - state == FIBER_DEAD on the pool, belt-and-braces against GC
 *     (which already skips non-RUNNABLE/NEW fibers).
 *   - Only the owning thread writes its freelist, mirroring the
 *     single-writer rule for fiber_list.
 *   - Default-sized only: the pool is a single slot class, not a
 *     size-indexed allocator.  Custom sizes fall through to the real
 *     mmap/munmap path. */

static int fiber_is_default_sized(const struct sb_fiber *f)
{
    static size_t dflt_stack, dflt_bind, dflt_arm64_lisp_stack;
    if (!dflt_stack) {
        size_t ps = os_reported_page_size;
        /* Match the formulas in sb_fiber_create / sb_fiber_lisp_stack_alloc. */
        dflt_stack = 3*STACK_GUARD_SIZE + align_up(FIBER_DEFAULT_STACK_SIZE, ps);
        dflt_bind  = align_up(FIBER_DEFAULT_BINDING_STACK_SIZE, ps);
        dflt_arm64_lisp_stack =
            align_up(FIBER_DEFAULT_STACK_SIZE, ps) + 3*STACK_GUARD_SIZE;
    }
    if (!f->stack_base) return 0; /* main fiber -- stacks not owned */
    if (f->stack_alloc_size         != dflt_stack) return 0;
    if (f->binding_stack_alloc_size != dflt_bind)  return 0;
#ifdef LISP_FEATURE_ARM64
    /* arm64's Lisp control stack has its own 3-guard layout at the top. */
    if (f->control_stack_alloc_size != dflt_arm64_lisp_stack)
        return 0;
#else
    (void)dflt_arm64_lisp_stack;
#endif
    return 1;
}

/* Scrub a fiber to a GC-safe idle state for the freelist.  Caller has
 * already unregistered from fiber_list.  Stack memory stays dirty --
 * sb_fiber_prepare rewrites the small header at pool-get, and GC
 * skips DEAD fibers entirely. */
static void fiber_pool_reset_for_put(struct sb_fiber *f)
{
    memset(&f->ctx, 0, sizeof(f->ctx));
    f->state = FIBER_DEAD;
    f->binding_stack_pointer = f->binding_stack_base;
    f->current_catch_block = 0;
    f->current_unwind_protect_block = 0;
    f->entry_fn = NULL;
    f->entry_arg = NULL;
    f->return_fiber = NULL;
#ifdef LISP_FEATURE_ARM64
    f->control_stack_pointer = f->control_stack_base;
    f->control_frame_pointer = f->control_stack_base;
#endif
    /* Restore the overflow guards to the default state: SOFT
     * PROT_NONE, RETURN R+W, cs_guard_protected = 1.  A pooled
     * fiber whose guard was lowered during its last life would
     * otherwise hand out a stack with the SOFT page unprotected
     * and the RETURN page armed. */
    if (!f->cs_guard_protected && f->stack_base) {
        size_t guard = STACK_GUARD_SIZE;
        mprotect((char *)f->stack_base + guard,   guard, PROT_NONE);
        mprotect((char *)f->stack_base + 2*guard, guard, PROT_READ | PROT_WRITE);
        f->cs_guard_protected = 1;
    }
}

/* Revive a pooled fiber.  Lisp will %fiber-register + %fiber-prepare
 * it like a fresh mmap'd one. */
static void fiber_pool_reset_for_get(struct sb_fiber *f)
{
    f->state = FIBER_NEW;
    f->next = NULL;
    f->owner = NULL;
}

/* Unmap a fiber's stacks and free its struct.  Used by both the
 * normal destroy path and the pool drain. */
static void fiber_release(struct sb_fiber *f)
{
    if (f->stack_base && f->stack_base != MAP_FAILED)
        munmap(f->stack_base, f->stack_alloc_size);
    if (f->binding_stack_base)
        munmap(f->binding_stack_base, f->binding_stack_alloc_size);
    sb_fiber_lisp_stack_free(f);
    free(f);
}

/* Release every fiber on a thread's freelist.  Pool members are
 * already unregistered, so this just bulk-releases them. */
void sb_fiber_pool_drain(struct thread *th)
{
    struct extra_thread_data *ed = thread_extra_data(th);
    struct sb_fiber *f = ed->fiber_freelist;
    ed->fiber_freelist = NULL;
    ed->fiber_freelist_count = 0;
    while (f) {
        struct sb_fiber *next = f->next;
        fiber_release(f);
        f = next;
    }
}

/* --- Allocation --- */

struct sb_fiber *sb_fiber_create(size_t stack_size,
                                 size_t binding_stack_size)
{
    size_t ps = os_reported_page_size;

    /* Freelist fast path: if the caller asked for the canonical default
     * sizes and the current thread has a parked fiber on its freelist,
     * revive and return it -- no mmap traffic.  Non-default sizes fall
     * through to the real allocation path below. */
    if ((stack_size == 0 || stack_size == FIBER_DEFAULT_STACK_SIZE)
        && (binding_stack_size == 0
            || binding_stack_size == FIBER_DEFAULT_BINDING_STACK_SIZE)) {
        struct thread *th = get_sb_vm_thread();
        if (th) {
            struct extra_thread_data *ed = thread_extra_data(th);
            if (ed->fiber_freelist) {
                struct sb_fiber *f = ed->fiber_freelist;
                ed->fiber_freelist = f->next;
                ed->fiber_freelist_count--;
                fiber_pool_reset_for_get(f);
                return f;
            }
        }
    }

    struct sb_fiber *f = calloc(1, sizeof(struct sb_fiber));
    if (!f) return NULL;

    /* Control stack: three STACK_GUARD_SIZE guard regions at the low
     * end followed by the usable region.  Stack grows downward on
     * both supported architectures, so overflow proceeds from high to
     * low addresses through the guards.  The layout mirrors what
     * SBCL's per-thread stack-overflow machinery expects
     * (validate.h: HARD, SOFT, RETURN, all sized STACK_GUARD_SIZE),
     * so handle_guard_page_triggered can treat a fiber's overflow
     * exactly like a thread's once sb_fiber_lisp_stack_{suspend,
     * resume} install the fiber's stack_base as
     * th->control_stack_start.
     *
     *   [stack_base    ... + G)   HARD guard   (PROT_NONE always)
     *   [+ G           ... + 2G)  SOFT guard   (PROT_NONE initially;
     *                                           lowered on overflow)
     *   [+ 2G          ... + 3G)  RETURN guard (R+W initially;
     *                                           protected while SOFT
     *                                           is lowered)
     *   [+ 3G          ... + 3G+N) usable region
     *
     * G = STACK_GUARD_SIZE.  On most builds this equals the OS page
     * size, but SBCL sets os_vm_page_size = BACKEND_PAGE_BYTES which
     * can be larger (e.g. 32 KiB).  The macros in validate.h derive
     * guard addresses from that quantity, so the fiber layout has to
     * match.
     */
    size_t guard = STACK_GUARD_SIZE;
    stack_size = align_up(stack_size ? stack_size : 65536, ps);
    f->stack_alloc_size = 3*guard + stack_size;
    f->stack_base = mmap(NULL, f->stack_alloc_size,
                         PROT_READ | PROT_WRITE,
                         MAP_PRIVATE | MAP_ANONYMOUS | MAP_STACK,
                         -1, 0);
    if (f->stack_base == MAP_FAILED) { free(f); return NULL; }

    /* HARD + SOFT both PROT_NONE; RETURN (region 2) stays R+W. */
    mprotect(f->stack_base,                    guard, PROT_NONE);
    mprotect((char *)f->stack_base + guard,    guard, PROT_NONE);
    f->stack_start = (char *)f->stack_base + 3*guard;
    f->stack_end   = (char *)f->stack_base + f->stack_alloc_size;
    f->cs_guard_protected = 1;

    /* Binding stack */
    binding_stack_size = align_up(
        binding_stack_size ? binding_stack_size : 8192, ps);
    f->binding_stack_alloc_size = binding_stack_size;
    f->binding_stack_base = mmap(NULL, binding_stack_size,
                                 PROT_READ | PROT_WRITE,
                                 MAP_PRIVATE | MAP_ANONYMOUS,
                                 -1, 0);
    if (f->binding_stack_base == MAP_FAILED) {
        munmap(f->stack_base, f->stack_alloc_size);
        free(f);
        return NULL;
    }
    f->binding_stack_end = (lispobj *)((char *)f->binding_stack_base
                                       + binding_stack_size);
    f->binding_stack_pointer = f->binding_stack_base;
    /* For child fibers, the value published in
     * thread->binding_stack_start while we run is just our own mmap'd
     * base. */
    f->binding_stack_start_for_thread = f->binding_stack_base;

    /* Arch-specific Lisp control stack setup.  On arm64 this mmaps a
     * separate Lisp stack; on x86-64 it aliases control_stack_* to the
     * native C stack bounds so *CONTROL-STACK-START/END* track the
     * running fiber. */
    if (sb_fiber_lisp_stack_alloc(f, stack_size) != 0) {
        munmap(f->stack_base, f->stack_alloc_size);
        munmap(f->binding_stack_base, f->binding_stack_alloc_size);
        free(f);
        return NULL;
    }
    /* calloc zeros state (= FIBER_NEW), catch/unwind, next, owner,
     * and return_fiber, so no explicit init needed. */
    return f;
}

/* Create a "main fiber" representing the thread's own stack.  No
 * stacks are owned; the thread's are the ground truth, and
 * binding_stack_base stays NULL so binding-stack swapping is skipped
 * for it.  calloc zeros the rest; we only set fields with a non-zero
 * value tied to the current thread. */
struct sb_fiber *sb_fiber_create_main(struct thread *th)
{
    struct sb_fiber *f = calloc(1, sizeof(struct sb_fiber));
    if (!f) return NULL;
    f->state = FIBER_RUNNING;
    f->binding_stack_pointer          = get_binding_stack_pointer(th);
    f->binding_stack_start_for_thread = th->binding_stack_start;
    f->current_catch_block            = th->current_catch_block;
    f->current_unwind_protect_block   = th->current_unwind_protect_block;
    /* Main fiber inherits the thread's current overflow-guard state
     * so a switch back to main after an overflow on a child fiber
     * restores the thread's bit to whatever it was at make-main-
     * fiber time (ordinarily 1 = guard protected). */
    f->cs_guard_protected = th->state_word.control_stack_guard_page_protected;
    sb_fiber_lisp_stack_capture_main(f, th);
    return f;
}

void sb_fiber_destroy(struct sb_fiber *f)
{
    if (!f) return;
    assert(f->state != FIBER_RUNNING);

    /* Capture owner before unregister nulls it out -- we need the
     * address of the owner thread's extra_data for pool eligibility
     * below. */
    struct thread *owner = f->owner;
    if (owner) sb_fiber_unregister(owner, f);

    /* Freelist fast path: park the fiber on its owning thread's
     * freelist if it's default-sized, the caller is running on the
     * owning thread (single-writer invariant), and the pool isn't
     * full.  Otherwise fall through to real release. */
    if (owner && owner == get_sb_vm_thread() && fiber_is_default_sized(f)) {
        struct extra_thread_data *ed = thread_extra_data(owner);
        if (ed->fiber_freelist_count < FIBER_POOL_MAX) {
            fiber_pool_reset_for_put(f);
            f->next = ed->fiber_freelist;
            ed->fiber_freelist = f;
            ed->fiber_freelist_count++;
            return;
        }
    }

    fiber_release(f);
}

/* --- GC registration --- */

void sb_fiber_register(struct thread *th, struct sb_fiber *fiber)
{
    assert(fiber->owner == NULL);
    fiber->owner = th;
    /* Prepend to thread's fiber list.
     * No lock needed: only the owning thread mutates its own list,
     * and GC holds the world stopped when it reads the list. */
    fiber->next = thread_extra_data(th)->fiber_list;
    __atomic_store_n(&thread_extra_data(th)->fiber_list,
                     fiber, __ATOMIC_RELEASE);
}

void sb_fiber_unregister(struct thread *th, struct sb_fiber *fiber)
{
    struct sb_fiber **pp = &thread_extra_data(th)->fiber_list;
    while (*pp) {
        if (*pp == fiber) {
            *pp = fiber->next;
            fiber->next = NULL;
            fiber->owner = NULL;
            return;
        }
        pp = &(*pp)->next;
    }
}

/* --- Context switch helpers (forward declarations for trampoline) --- */

static inline void swap_bindings_forward (struct thread *th,
                                          lispobj *base, lispobj *limit);
static inline void swap_bindings_backward(struct thread *th,
                                          lispobj *base, lispobj *limit);

/* --- New fiber bootstrap ---
 *
 * Invoked from the per-arch asm stub fiber_trampoline_asm after the
 * stub has moved the saved fiber-pointer register into the first
 * argument register. */
void fiber_trampoline_c(struct sb_fiber *self)
{
    struct thread *th = get_sb_vm_thread();
    /* We were just resumed via a VOP (or fiber_swap_context) that
     * ran inside a pseudo-atomic region entered by whoever switched
     * to us.  They never get to exit PA -- the register swap
     * transferred the CPU to us before they could -- so the exit is
     * our job before we run user code.  sb_fiber_switch_prep already
     * flipped self to RUNNING on that switch, so we don't set it. */
    sb_fiber_exit_pa(th);
#ifdef LISP_FEATURE_ARM64
    /* arm64 call-out-to-C postamble (see call_into_c in
     * arm64-assem.S) clears thread->control_stack_pointer on the
     * return from sb_fiber_switch_prep's alien call -- its "back
     * in Lisp" FFCA marker.  But we've since swapped out of that
     * Lisp's stack, and the alien-callable trampoline that
     * self->entry_fn resolves to reads this slot from call_into_lisp
     * to initialize reg_CSP for the new fiber's first Lisp
     * frame.  sb_fiber_lisp_stack_resume seeded self's saved slots
     * with the fiber's base; reinstate them here before the
     * callback runs. */
    th->control_stack_pointer = self->control_stack_pointer;
    th->control_frame_pointer = self->control_frame_pointer;
#endif
    self->entry_fn(self->entry_arg);
    self->state = FIBER_DEAD;

    /* Auto-switch back to the return fiber if one was set.  We run
     * outside Lisp here, so the catch/unwind bookkeeping that the
     * Lisp-side fiber-switch shim normally owns has to be done in
     * C.  Prep enters pseudo-atomic and handles the state staging;
     * fiber_swap_context then does the register/SP swap.  We never
     * reach the hypothetical post-swap exit-PA because our stack is
     * abandoned when self is DEAD -- the resuming fiber handles its
     * own exit-PA via its %fiber-register-swap VOP's tail. */
    if (self->return_fiber) {
        struct thread *th = get_sb_vm_thread();
        struct sb_fiber *ret = self->return_fiber;

        th->current_catch_block          = ret->current_catch_block;
        th->current_unwind_protect_block = ret->current_unwind_protect_block;

        sb_fiber_switch_prep(self, ret);
        fiber_swap_context(&self->ctx, &ret->ctx);
    }
    /* Should never reach here -- either return_fiber switched us away,
     * or there was no return_fiber. */
    for (;;) __builtin_trap();
}

/* --- Context switch --- */

/* Swap TLS values with a fiber's binding-stack entries.
 *
 * Each binding-stack entry stores (tls-index, old-value) -- "old-value"
 * being the TLS value the symbol held immediately before this binding
 * was pushed.  A chain of N bindings of the same symbol therefore
 * threads a chain of old values: entry k stores V_{k-1}, and the live
 * TLS slot holds V_N.
 *
 * The swap operation exchanges each entry's stored value with the live
 * TLS value.  Because every swap mutates the stored value, the direction
 * of iteration matters whenever the same symbol is bound more than once
 * on the same stack:
 *
 *   - To UNDO the fiber's bindings (restore TLS to the pre-binding state
 *     V_0), entries must be visited top-down, i.e. most-recent first.
 *     After the top-down pass, entry k holds V_k, and TLS = V_0.
 *
 *   - To REDO those same bindings (reinstate TLS = V_N), entries must be
 *     visited bottom-up, which reverses the prior pass and restores the
 *     original (V_{k-1}) contents.
 *
 * A single symmetric function iterating bottom-up on both unbind and
 * rebind is INCORRECT for nested bindings of the same special variable;
 * that was the original sb-fiber handler-chain bug (IMPL-344).
 *
 * Indirect-cell maintenance (TLS-LOAD-INDIRECT only).
 *
 * On builds with :tls-load-indirect (silently enabled for sb-thread
 * x86-64 in src/cold/shared.lisp), each special variable occupies two
 * words in the thread TLS area: a value slot at thread+tls_index, and
 * an "indirect cell" at thread+tls_index-8 that the compiled symbol-
 * value access path dereferences.  The reader emits
 *
 *     MOV reg, [thread + tls_index - 8]   ; indirect cell
 *     MOV reg, [reg + 1]                  ; -> value
 *
 * with NO NO_TLS_VALUE_MARKER fallback in the read path.  The SBCL
 * BIND vop sets the indirect cell to (thread+tls_index-1) so [+1]
 * reaches the value slot; the UNBIND vop, if the captured old value
 * was NO_TLS_VALUE_MARKER, rewrites the cell to the tagged symbol so
 * [+1] reaches the symbol's value-slot (the global value).
 *
 * swap_one only swaps the value slot, so after swap-out the indirect
 * cell can be left pointing at a TLS slot that now holds
 * NO_TLS_VALUE_MARKER, and any subsequent read returns the marker as
 * if it were the value.  We mirror the BIND/UNBIND policy here:
 * after writing the value slot, set the indirect cell to the symbol
 * (when the slot is NO_TLS_VALUE_MARKER) or back to thread+tls_index-1
 * (otherwise).  Symbol lookup uses tlsindex_to_symbol_map, the table
 * the runtime already maintains for trap handling and GC.
 */
#ifdef LISP_FEATURE_TLS_LOAD_INDIRECT
extern lispobj *tlsindex_to_symbol_map;
extern int tls_map_starting_offset;
#  ifndef NO_TLS_VALUE_MARKER
#    define NO_TLS_VALUE_MARKER (~(uword_t)0)
#  endif
#endif

static inline void swap_one(struct thread *th, struct binding *b)
{
    if (b->symbol && b->symbol != UNBOUND_MARKER_WIDETAG) {
#ifdef LISP_FEATURE_SB_THREAD
        lispobj *tls_slot = (lispobj *)(b->symbol + (char *)th);
#else
        lispobj *tls_slot = &SYMBOL(b->symbol)->value;
#endif
        lispobj tmp = *tls_slot;
        *tls_slot = b->value;
        b->value = tmp;

#ifdef LISP_FEATURE_TLS_LOAD_INDIRECT
        /* Indirect-cell maintenance is only required for symbols that
         * the compiler actually accesses via the double-deref code path.
         * Those are the symbols whose tls_index is at or above
         * tls_map_starting_offset (which equals *PACKAGE*'s tls-index).
         *
         * Symbols below that threshold are :always-thread-local (wired
         * TLS) -- the BIND vop reads/writes their value slot directly
         * and deliberately leaves the "would-be" indirect cell alone
         * (see SYMBOL-ALWAYS-HAS-TLS-VALUE-P handling in
         * x86-64/tls.lisp).  Their tls_index may land at offset 8 in a
         * 16-byte chunk (e.g. *RESTART-CLUSTERS* at 488), in which case
         * tls_index-8 happens to be another always-thread-local
         * symbol's value slot (*GC-PIN-CODE-PAGES* at 480).  Writing a
         * routing pointer there silently clobbers that symbol's value
         * -- which is exactly what crashed RUN-TESTS::
         * COMPARE-SYMBOL-VALUES during the fiber test suite. */
        if (b->symbol >= (lispobj)tls_map_starting_offset) {
            lispobj *indirect_cell =
                (lispobj *)((char *)th + b->symbol - N_WORD_BYTES);
            if (*tls_slot == (lispobj)NO_TLS_VALUE_MARKER) {
                lispobj symbol =
                    tlsindex_to_symbol_map[b->symbol >> (1 + WORD_SHIFT)];
                /* If the map has no entry (slot allocated post-dump,
                 * or weak-cell-cleared), fall back to "value lives in
                 * TLS slot" routing -- harmless even though TLS holds
                 * the marker. */
                *indirect_cell =
                    (symbol == (lispobj)NO_TLS_VALUE_MARKER)
                    ? (lispobj)((char *)th + b->symbol - 1)
                    : symbol;
            } else {
                *indirect_cell = (lispobj)((char *)th + b->symbol - 1);
            }
        }
#endif
    }
}

/* Redo / swap-in direction: iterate oldest -> newest. */
static inline void swap_bindings_forward(struct thread *th,
                                         lispobj *base, lispobj *limit)
{
    struct binding *b = (struct binding *)base;
    struct binding *end = (struct binding *)limit;
    for (; b < end; b++)
        swap_one(th, b);
}

/* Undo / swap-out direction: iterate newest -> oldest. */
static inline void swap_bindings_backward(struct thread *th,
                                          lispobj *base, lispobj *limit)
{
    struct binding *start = (struct binding *)base;
    struct binding *b     = (struct binding *)limit;
    while (b > start) {
        b--;
        swap_one(th, b);
    }
}

/* --- Split switch: prep + VOP register swap, plus an exit-PA epilogue
 * emitted at the VOP's RESUME tail. ---
 *
 * Signal safety: the critical region -- prep's BSP/bounds/state/
 * binding-swap plus the register/SP swap itself -- runs inside a
 * pseudo-atomic region.  Any deferrable signal firing inside the
 * region sets the PA_INTERRUPTED flag in pa_bits and returns; the
 * exit-PA sequence clears PA_IN, checks PA_INTERRUPTED, and traps
 * to dispatch the deferred handler.  This replaces the old
 * block_deferrable_signals/thread_sigmask pair, which cost
 * ~2.4 us per switch in sigprocmask syscalls.
 *
 * The pa_bits encoding is arch-specific (see pseudo-atomic.h):
 *   - x86-64: a single 64-bit word; in-PA iff any bit above bit 0
 *     is set, with bit 0 reserved for PA_INTERRUPTED.  Writing
 *     (uword_t)th is the canonical "in PA" value.
 *   - arm64:  two 32-bit halves; PA_IN occupies the low half
 *     (nonzero with flag_PseudoAtomic low bits set), PA_INTERRUPTED
 *     the high half (set by the signal handler as
 *     flag_PseudoAtomicInterrupted).  Entry writes flag_PseudoAtomic
 *     into the low half only, leaving the high half untouched.
 *
 * The PA window spans two fibers' stacks: PA is entered here on the
 * SUSPENDING side, and exited on the RESUMING side (in the
 * %fiber-register-swap VOP's tail, right after its RESUME label, or
 * at the top of fiber_trampoline_c for a new fiber's first
 * entry).  Because pa_bits lives on the thread -- not the fiber --
 * this works as long as every suspend is paired with exactly one
 * resume-side exit-PA on the same thread.
 *
 * BSP save stays in C because ALIEN-FUNCALL on c-stack-is-control-
 * stack platforms can wrap the call in INVOKE-WITH-SAVED-FP, which
 * binds *SAVED-FP* across the call.  Reading BSP from Lisp before
 * the alien call would miss that binding; the swap walk would then
 * run over a stale range, and UNBIND on return would restore TLS
 * from the wrong slot.  Reading it here captures BSP after any
 * wrapper's BIND.  (The contrib actually suppresses that wrap for
 * prep -- see fiber.lisp -- but the comment still applies should it
 * be reintroduced.)
 *
 * State transitions (from->RUNNABLE, to->RUNNING) happen here and
 * not in the Lisp shim because they must be synchronized with the
 * th->control_stack_* swap.  scan_fiber_stacks skips fibers in state
 * RUNNING on the assumption that their stack is covered by
 * conservative_stack_scan via th->control_stack_*.  If the state
 * flipped to RUNNING before th->cs_* was swapped to match, a GC
 * firing in the gap would scan the wrong stack and drop live roots
 * on the to-fiber's suspended Lisp stack.  Doing both in prep,
 * inside the PA region, closes that window. */

static inline void fiber_enter_pa(struct thread *th)
{
#if defined LISP_FEATURE_ARM64
    /* Mirror the arm64 pseudo-atomic Lisp macro, which stores a NIL-
     * tagged value (low 3 bits = flag_PseudoAtomic = 7) into the low
     * 32-bit half only, leaving the high half available for the
     * signal handler's PA_INTERRUPTED flag. */
    ((volatile uint32_t *)&th->pseudo_atomic_bits)[0] = flag_PseudoAtomic;
#else
    /* x86-64 convention: the whole word is nonzero while in PA, with
     * bit 0 reserved for PA_INTERRUPTED.  Thread is aligned, so
     * (uword_t)th has bit 0 clear and matches the Lisp macro. */
    th->pseudo_atomic_bits = (uword_t)th;
#endif
}

void sb_fiber_switch_prep(struct sb_fiber *from, struct sb_fiber *to)
{
    struct thread *th = get_sb_vm_thread();
    fiber_enter_pa(th);

    from->binding_stack_pointer = get_binding_stack_pointer(th);
    set_binding_stack_pointer(th, to->binding_stack_pointer);
    /* Track the active fiber's bindings region in the thread slot so
     * that *BINDING-STACK-START*-consulting code (binding-stack-usage,
     * walk-binding-stack, ...) sees the right base. */
    th->binding_stack_start = to->binding_stack_start_for_thread;

    sb_fiber_lisp_stack_suspend(from, th);
    sb_fiber_lisp_stack_resume (to,   th);
    /* Only RUNNING->RUNNABLE is written: fiber_trampoline_c's auto-
     * return path calls prep with from already in state DEAD, which
     * must be preserved. */
    if (from->state == FIBER_RUNNING) from->state = FIBER_RUNNABLE;
    to->state = FIBER_RUNNING;

    if (from->binding_stack_base)
        swap_bindings_backward(th, from->binding_stack_base,
                               from->binding_stack_pointer);
    if (to->binding_stack_base)
        swap_bindings_forward (th, to->binding_stack_base,
                               to->binding_stack_pointer);
}

/* Exit pseudo-atomic and dispatch any interrupt the signal handler
 * deferred during the PA region.  Used by fiber_trampoline_c to exit
 * PA at the top of a new fiber and around the auto-return-to-
 * return_fiber swap; Lisp-initiated switches exit PA via inline asm
 * in the %fiber-register-swap VOP tail, so they don't come through
 * here. */
void sb_fiber_exit_pa(struct thread *th)
{
#if defined LISP_FEATURE_ARM64
    /* Clear PA_IN (low 32 bits) then inspect PA_INTERRUPTED (high
     * 32 bits).  If set, clear it and BRK with trap_PendingInterrupt
     * -- the SIGTRAP handler decodes that into do_pending_interrupt. */
    volatile uint32_t *halves = (volatile uint32_t *)&th->pseudo_atomic_bits;
    halves[0] = 0;
    if (halves[1]) {
        halves[1] = 0;
        asm volatile("brk %0" : : "i"(trap_PendingInterrupt));
    }
#else
    /* x86-64: XOR pa_bits with the thread address.  If no interrupt
     * arrived the result is 0 (PA_IN cleared cleanly); otherwise bit
     * 0 remains set and UD2 traps to dispatch the handler. */
    uword_t pa = __sync_xor_and_fetch(&th->pseudo_atomic_bits, (uword_t)th);
    if (pa) {
#if defined LISP_FEATURE_UD2_BREAKPOINTS
        asm volatile("ud2\n\t.byte %c0" : : "i"(trap_PendingInterrupt));
#else
        asm volatile("ud2");
#endif
    }
#endif
}

/* Byte offsets of struct sb_fiber fields that the Lisp-side
 * fiber-switch shim reads and writes via SAP-REF.  Queried at load
 * time through sb_fiber_struct_offset (below) so the contrib never
 * hardcodes a per-arch layout. */
enum sb_fiber_field {
    SB_FIBER_FIELD_STATE         = 0,
    SB_FIBER_FIELD_OWNER         = 1,
    SB_FIBER_FIELD_BSP           = 2,
    SB_FIBER_FIELD_CATCH         = 3,
    SB_FIBER_FIELD_UNWIND        = 4,
    SB_FIBER_FIELD_RETURN_FIBER  = 5
};

int sb_fiber_struct_offset(int field)
{
    switch (field) {
    case SB_FIBER_FIELD_STATE:
        return (int)offsetof(struct sb_fiber, state);
    case SB_FIBER_FIELD_OWNER:
        return (int)offsetof(struct sb_fiber, owner);
    case SB_FIBER_FIELD_BSP:
        return (int)offsetof(struct sb_fiber, binding_stack_pointer);
    case SB_FIBER_FIELD_CATCH:
        return (int)offsetof(struct sb_fiber, current_catch_block);
    case SB_FIBER_FIELD_UNWIND:
        return (int)offsetof(struct sb_fiber, current_unwind_protect_block);
    case SB_FIBER_FIELD_RETURN_FIBER:
        return (int)offsetof(struct sb_fiber, return_fiber);
    default:
        return -1;
    }
}
