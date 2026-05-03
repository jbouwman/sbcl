#include "fiber.h"
#include "os.h"
#include "globals.h"
#include "thread.h"
#include "validate.h"
#include "interr.h"
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
 * See README.md "Freelist" for invariants. */

static int fiber_is_default_sized(const struct sb_fiber *f)
{
    static size_t dflt_stack, dflt_bind;
    if (!dflt_stack) {
        size_t ps = os_reported_page_size;
        dflt_stack = 3*STACK_GUARD_SIZE + align_up(FIBER_DEFAULT_STACK_SIZE, ps);
        dflt_bind  = align_up(FIBER_DEFAULT_BINDING_STACK_SIZE, ps);
    }
    if (!f->stack_base) return 0; /* main fiber -- stacks not owned */
    if (f->stack_alloc_size         != dflt_stack) return 0;
    if (f->binding_stack_alloc_size != dflt_bind)  return 0;
    return 1;
}

/* Scrub a fiber to FIBER_DEAD idle state for the pool.  Caller has
 * already unregistered.  Stack contents stay dirty -- sb_fiber_prepare
 * rewrites the trampoline header on revive, and GC skips DEAD. */
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
    /* Re-arm CS SOFT (PROT_NONE) and disarm CS RETURN (R+W) if a
     * prior overflow lowered the control-stack guards. */
    if (!f->cs_guard_protected && f->stack_base) {
        size_t guard = STACK_GUARD_SIZE;
        mprotect((char *)f->stack_base + guard,   guard, PROT_NONE);
        mprotect((char *)f->stack_base + 2*guard, guard, PROT_READ | PROT_WRITE);
        f->cs_guard_protected = 1;
    }
    /* Same for binding-stack guards. */
    if (!f->bs_guard_protected && f->binding_stack_base) {
        sb_fiber_reset_bs_guard(f);
    }
}

static void fiber_pool_reset_for_get(struct sb_fiber *f)
{
    f->state = FIBER_NEW;
    f->next = NULL;
    f->owner = NULL;
}

static void fiber_release(struct sb_fiber *f)
{
    if (f->stack_base && f->stack_base != MAP_FAILED)
        munmap(f->stack_base, f->stack_alloc_size);
    if (f->binding_stack_base)
        munmap(f->binding_stack_base, f->binding_stack_alloc_size);
    sb_fiber_lisp_stack_free(f);
    free(f);
}

/* Release every fiber on a thread's freelist.  Called from
 * free_thread_struct on thread exit. */
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

/* Release every fiber still on the registered fiber_list.  Called
 * from free_thread_struct so a thread that exits without
 * destroy-fibering its children doesn't leak their mmap'd stacks. */
void sb_fiber_release_registered(struct thread *th)
{
    struct extra_thread_data *ed = thread_extra_data(th);
    struct sb_fiber *f = ed->fiber_list;
    ed->fiber_list = NULL;
    while (f) {
        struct sb_fiber *next = f->next;
        f->owner = NULL;
        f->next = NULL;
        fiber_release(f);
        f = next;
    }
}

/* --- Allocation --- */

struct sb_fiber *sb_fiber_create(size_t stack_size,
                                 size_t binding_stack_size)
{
    size_t ps = os_reported_page_size;

    /* Freelist fast path: revive a parked default-sized fiber if one
     * is available.  See README.md "Freelist". */
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

    /* Control stack: 3 * STACK_GUARD_SIZE guards (HARD/SOFT/RETURN)
     * at the low end, then the usable region.  See README.md "Stack
     * layout". */
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

    /* Binding stack: usable | RETURN | SOFT | HARD, matching the
     * thread's triple-guard layout in validate.h. */
    binding_stack_size = align_up(
        binding_stack_size ? binding_stack_size : 8192, ps);
    f->binding_stack_alloc_size = binding_stack_size + 3 * ps;
    f->binding_stack_base = mmap(NULL, f->binding_stack_alloc_size,
                                 PROT_READ | PROT_WRITE,
                                 MAP_PRIVATE | MAP_ANONYMOUS,
                                 -1, 0);
    if (f->binding_stack_base == MAP_FAILED) {
        munmap(f->stack_base, f->stack_alloc_size);
        free(f);
        return NULL;
    }
    mprotect((char *)f->binding_stack_base + binding_stack_size + ps,
             2 * ps, PROT_NONE);
    f->binding_stack_end = (lispobj *)((char *)f->binding_stack_base
                                       + binding_stack_size);
    f->binding_stack_pointer = f->binding_stack_base;
    f->binding_stack_start_for_thread = f->binding_stack_base;
    f->bs_guard_protected = 1;

    /* Alias control_stack_* to the native C stack range. */
    if (sb_fiber_lisp_stack_alloc(f, stack_size) != 0) {
        munmap(f->stack_base, f->stack_alloc_size);
        munmap(f->binding_stack_base, f->binding_stack_alloc_size);
        free(f);
        return NULL;
    }
    /* state == FIBER_NEW and the rest from calloc. */
    return f;
}

/* Main fiber: represents the thread's own stack.  Owns nothing;
 * binding_stack_base stays NULL so the binding-stack swap skips it. */
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

    /* Capture owner before unregister nulls it out. */
    struct thread *owner = f->owner;
    if (owner) sb_fiber_unregister(owner, f);

    /* Park on the owning thread's freelist if eligible. */
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
    /* Prepend.  Single-writer (owning thread); RELEASE pairs with
     * the GC's STW-side read of fiber_list. */
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

static inline void swap_bindings_forward (struct thread *th,
                                          lispobj *base, lispobj *limit);
static inline void swap_bindings_backward(struct thread *th,
                                          lispobj *base, lispobj *limit);

/* --- Binding-stack guard manipulation --- */

void sb_fiber_lower_bs_guard(struct sb_fiber *f)
{
    size_t ps = os_reported_page_size;
    char *base = (char *)f->binding_stack_base;
    size_t usable = f->binding_stack_alloc_size - 3 * ps;
    mprotect(base + usable + ps, ps, PROT_READ | PROT_WRITE);
    mprotect(base + usable,      ps, PROT_NONE);
    f->bs_guard_protected = 0;
}

void sb_fiber_reset_bs_guard(struct sb_fiber *f)
{
    size_t ps = os_reported_page_size;
    char *base = (char *)f->binding_stack_base;
    size_t usable = f->binding_stack_alloc_size - 3 * ps;
    mprotect(base + usable + ps, ps, PROT_NONE);
    mprotect(base + usable,      ps, PROT_READ | PROT_WRITE);
    f->bs_guard_protected = 1;
}

int sb_fiber_classify_bs_fault(struct thread *th, void *addr,
                               struct sb_fiber **out_fiber)
{
    size_t ps = os_reported_page_size;
    char *a = (char *)addr;
    for (struct sb_fiber *f = thread_extra_data(th)->fiber_list;
         f; f = f->next) {
        if (!f->binding_stack_base) continue;
        char *base = (char *)f->binding_stack_base;
        size_t usable = f->binding_stack_alloc_size - 3 * ps;
        char *r = base + usable, *s = r + ps, *h = s + ps;
        if (a >= h && a < h + ps) { if (out_fiber) *out_fiber = f; return 1; }
        if (a >= s && a < s + ps) { if (out_fiber) *out_fiber = f; return 2; }
        if (a >= r && a < r + ps) { if (out_fiber) *out_fiber = f; return 3; }
    }
    return 0;
}

/* New-fiber entry: called from fiber_trampoline_asm after it moves
 * the fiber pointer into the first argument register.  See README.md
 * "Trampoline auto-return". */
void fiber_trampoline_c(struct sb_fiber *self)
{
    struct thread *th = get_sb_vm_thread();
    /* Whoever switched to us entered PA but couldn't exit it (the
     * register swap transferred control mid-region); we exit on
     * their behalf. */
    sb_fiber_exit_pa(th);
    self->entry_fn(self->entry_arg);
    self->state = FIBER_DEAD;

    if (self->return_fiber) {
        struct thread *th = get_sb_vm_thread();
        struct sb_fiber *ret = self->return_fiber;

        th->current_catch_block          = ret->current_catch_block;
        th->current_unwind_protect_block = ret->current_unwind_protect_block;

        sb_fiber_switch_prep(self, ret);
        fiber_swap_context(&self->ctx, &ret->ctx);
    }
    lose("fiber_trampoline_c reached past auto-return");
}

/* --- Binding-stack swap ---
 * See README.md "Binding-stack swap" for the algorithm and the
 * TLS-LOAD-INDIRECT cell-maintenance rule.  The two passes below
 * differ only in iteration direction.
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
        /* Maintain the indirect cell for symbols at or above the
         * map-starting offset; symbols below are :always-thread-local
         * and their would-be cell may overlap another symbol's value
         * slot (see README "TLS-LOAD-INDIRECT" for details). */
        if (b->symbol >= (lispobj)tls_map_starting_offset) {
            lispobj *indirect_cell =
                (lispobj *)((char *)th + b->symbol - N_WORD_BYTES);
            if (*tls_slot == (lispobj)NO_TLS_VALUE_MARKER) {
                lispobj symbol =
                    tlsindex_to_symbol_map[b->symbol >> (1 + WORD_SHIFT)];
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

/* --- Pseudo-atomic switch window ---
 * See README.md "Pseudo-atomic switch window" for the protocol.
 * BSP capture lives here (not in the Lisp shim) so the read happens
 * after any INVOKE-WITH-SAVED-FP wrapper around the alien call would
 * have established its binding.  The state flip and the
 * th->control_stack_* swap must stay paired inside PA; see
 * scan_fiber_stacks for the GC-side dependency.
 */

static inline void fiber_enter_pa(struct thread *th)
{
    th->pseudo_atomic_bits = (uword_t)th;
}

void sb_fiber_switch_prep(struct sb_fiber *from, struct sb_fiber *to)
{
    struct thread *th = get_sb_vm_thread();
    fiber_enter_pa(th);

    from->binding_stack_pointer = get_binding_stack_pointer(th);
    set_binding_stack_pointer(th, to->binding_stack_pointer);
    th->binding_stack_start = to->binding_stack_start_for_thread;

    sb_fiber_lisp_stack_suspend(from, th);
    sb_fiber_lisp_stack_resume (to,   th);
    /* DEAD must survive: trampoline auto-return calls prep with
     * from already DEAD. */
    if (from->state == FIBER_RUNNING) from->state = FIBER_RUNNABLE;
    to->state = FIBER_RUNNING;

    if (from->binding_stack_base)
        swap_bindings_backward(th, from->binding_stack_base,
                               from->binding_stack_pointer);
    if (to->binding_stack_base)
        swap_bindings_forward (th, to->binding_stack_base,
                               to->binding_stack_pointer);
}

/* Exit pseudo-atomic and dispatch any deferred interrupt.  Used by
 * fiber_trampoline_c (new-fiber entry and auto-return);
 * Lisp-initiated switches inline this in the VOP's RESUME tail. */
void sb_fiber_exit_pa(struct thread *th)
{
    /* XOR pa_bits with the thread address.  If no interrupt arrived
     * the result is 0 (PA_IN cleared cleanly); otherwise bit 0 remains
     * set and UD2 traps to dispatch the handler. */
    uword_t pa = __sync_xor_and_fetch(&th->pseudo_atomic_bits, (uword_t)th);
    if (pa) {
#if defined LISP_FEATURE_UD2_BREAKPOINTS
        asm volatile("ud2\n\t.byte %c0" : : "i"(trap_PendingInterrupt));
#else
        asm volatile("ud2");
#endif
    }
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
