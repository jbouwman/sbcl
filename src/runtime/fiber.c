#include "fiber.h"
#include "os.h"
#include "globals.h"
#include "thread.h"
#include "validate.h"
#include "interr.h"
#include "interrupt.h"
#include "align.h"
#include "genesis/symbol.h"
#include "genesis/static-symbols.h"
#include "lispobj.h"
#include <sys/mman.h>
#include <sched.h>
#include <string.h>
#include <assert.h>
#include <stdint.h>

#ifndef MAP_STACK
#define MAP_STACK 0
#endif

extern void gc_preserve_fiber_word(lispobj word);
extern void gc_scav_fiber_binding_stack(lispobj *base, lispobj *end);

/* A thread's fiber list is a singly linked list whose unlink of an
 * interior element is a plain store to its predecessor's NEXT: correct
 * only while nothing else changes the list.  Migration changes a list
 * from a thread other than its owner, so every change takes the owner's
 * lock.  Holders run pseudo-atomic or with the blockable signals blocked
 * (see fiber.h), so a holder is never stopped for GC nor interrupted
 * into code that takes the same lock, and the sections are short. */
void sb_fiber_list_lock(struct thread *th)
{
    int *lock = &thread_extra_data(th)->fiber_list_lock;
    for (unsigned spins = 0;
         __atomic_exchange_n(lock, 1, __ATOMIC_ACQUIRE);
         spins++) {
        while (__atomic_load_n(lock, __ATOMIC_RELAXED)) {
            if (++spins > 1000) { sched_yield(); spins = 0; }
        }
    }
}

void sb_fiber_list_unlock(struct thread *th)
{
    __atomic_store_n(&thread_extra_data(th)->fiber_list_lock, 0,
                     __ATOMIC_RELEASE);
}

void gc_scan_fiber_stacks(struct thread *th)
{
    for (struct sb_fiber_ctx *f = thread_extra_data(th)->fiber_list;
         f; f = f->next) {
        if (f->state != FIBER_RUNNABLE && f->state != FIBER_NEW) continue;

        lispobj *hi = (lispobj *)f->stack_end;
#ifdef LISP_FEATURE_X86_64
        /* Main fiber: stack_end==NULL but Lisp frames live on the
         * thread's C stack captured at make-main-fiber time. */
        if (!hi) hi = f->control_stack_end;
#endif
        if (hi)
            for (lispobj *p = (lispobj *)sb_fiber_sp(f); p < hi; p++)
                gc_preserve_fiber_word(*p);

        lispobj regs[16];
        int n = sb_fiber_gc_regs(f, regs, 16);
        for (int i = 0; i < n; i++)
            gc_preserve_fiber_word(regs[i]);

#ifdef LISP_FEATURE_ARM64
        if (f->control_stack_base && f->control_stack_pointer
            && f->control_stack_pointer > f->control_stack_base)
            for (lispobj *p = f->control_stack_base;
                 p < f->control_stack_pointer; p++)
                gc_preserve_fiber_word(*p);
#endif
    }
}

void gc_scav_fiber_binding_stacks(struct thread *th)
{
    for (struct sb_fiber_ctx *f = thread_extra_data(th)->fiber_list;
         f; f = f->next) {
        if ((f->state == FIBER_RUNNABLE || f->state == FIBER_NEW)
            && f->binding_stack_base)
            gc_scav_fiber_binding_stack(f->binding_stack_base,
                                        f->binding_stack_pointer);
    }
}

uword_t sb_fiber_gc_epoch;

/* With the world stopped: no fiber switch is in progress, since a switch
 * runs pseudo-atomic. */
void sb_fiber_note_global_gc(void)
{
    sb_fiber_gc_epoch++;
}

static void fiber_free(struct sb_fiber_ctx *f)
{
    if (f->stack_base && f->stack_base != MAP_FAILED)
        munmap(f->stack_base, f->stack_alloc_size);
    if (f->binding_stack_base)
        munmap(f->binding_stack_base, f->binding_stack_alloc_size);
    sb_fiber_lisp_stack_free(f);
    free(f);
}

void sb_fiber_release_registered(struct thread *th)
{
    struct extra_thread_data *ed = thread_extra_data(th);
    sigset_t saved;
    block_blockable_signals(&saved);
    sb_fiber_list_lock(th);
    struct sb_fiber_ctx *f = ed->fiber_list;
    ed->fiber_list = NULL;
    sb_fiber_list_unlock(th);
    thread_sigmask(SIG_SETMASK, &saved, 0);
    while (f) {
        struct sb_fiber_ctx *next = f->next;
        f->owner = NULL;
        f->next = NULL;
        fiber_free(f);
        f = next;
    }
}

struct sb_fiber_ctx *sb_fiber_create(size_t stack_size,
                                 size_t binding_stack_size)
{
    size_t ps = os_reported_page_size;
    struct sb_fiber_ctx *f = calloc(1, sizeof(struct sb_fiber_ctx));
    if (!f) return NULL;

    size_t guard = STACK_GUARD_SIZE;
    stack_size = ALIGN_UP(stack_size ? stack_size : 65536, ps);
    f->stack_alloc_size = 3*guard + stack_size;
    f->stack_base = mmap(NULL, f->stack_alloc_size,
                         PROT_READ | PROT_WRITE,
                         MAP_PRIVATE | MAP_ANONYMOUS | MAP_STACK,
                         -1, 0);
    if (f->stack_base == MAP_FAILED) { free(f); return NULL; }

    mprotect(f->stack_base,                    guard, PROT_NONE);
    mprotect((char *)f->stack_base + guard,    guard, PROT_NONE);
    f->stack_start = (char *)f->stack_base + 3*guard;
    f->stack_end   = (char *)f->stack_base + f->stack_alloc_size;
    f->cs_guard_protected = 1;

    binding_stack_size = ALIGN_UP(
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

    if (sb_fiber_lisp_stack_alloc(f, stack_size) != 0) {
        munmap(f->stack_base, f->stack_alloc_size);
        munmap(f->binding_stack_base, f->binding_stack_alloc_size);
        free(f);
        return NULL;
    }
    return f;
}

/* main fiber wraps the thread's own stack. */
struct sb_fiber_ctx *sb_fiber_create_main(struct thread *th)
{
    struct sb_fiber_ctx *f = calloc(1, sizeof(struct sb_fiber_ctx));
    if (!f) return NULL;
    f->state = FIBER_RUNNING;
    f->binding_stack_pointer          = get_binding_stack_pointer(th);
    f->binding_stack_start_for_thread = th->binding_stack_start;
    f->current_catch_block            = th->current_catch_block;
    f->current_unwind_protect_block   = th->current_unwind_protect_block;
    f->cs_guard_protected = th->state_word.control_stack_guard_page_protected;
    sb_fiber_lisp_stack_capture_main(f, th);
    return f;
}

static int fiber_list_remove(struct thread *src, struct sb_fiber_ctx *fiber);
static void fiber_list_insert(struct thread *dest, struct sb_fiber_ctx *fiber);
static inline void sb_fiber_enter_pa(struct thread *th);

/* Take the lock of FIBER's owner, rechecking the owner under it: a
 * concurrent migration may move FIBER between the read and the lock.
 * Returns the owner with its lock held, or NULL, holding nothing, if
 * FIBER has none.  The caller is pseudo-atomic. */
static struct thread *fiber_lock_owner(struct sb_fiber_ctx *fiber)
{
    for (;;) {
        struct thread *owner = __atomic_load_n(&fiber->owner, __ATOMIC_ACQUIRE);
        if (!owner) return NULL;
        sb_fiber_list_lock(owner);
        if (__atomic_load_n(&fiber->owner, __ATOMIC_ACQUIRE) == owner)
            return owner;
        sb_fiber_list_unlock(owner);
    }
}

void sb_fiber_release(struct sb_fiber_ctx *f)
{
    if (!f) return;
    struct thread *self = get_sb_vm_thread();
    sb_fiber_enter_pa(self);
    struct thread *owner = fiber_lock_owner(f);
    if (owner) {
        if (thread_extra_data(owner)->current_fiber == f)
            thread_extra_data(owner)->current_fiber = NULL;
        if (fiber_list_remove(owner, f) == 0) {
            f->next = NULL;
            f->owner = NULL;
        }
        sb_fiber_list_unlock(owner);
    }
    sb_fiber_exit_pa(self);
    fiber_free(f);
}

/* Registering a fiber does not make it current, even a main fiber, which
 * is always RUNNING: sb_fiber_set_current does that. */
void sb_fiber_register(struct thread *th, struct sb_fiber_ctx *fiber)
{
    assert(fiber->owner == NULL);
    struct thread *self = get_sb_vm_thread();
    sb_fiber_enter_pa(self);
    sb_fiber_list_lock(th);
    fiber->owner = th;
    fiber_list_insert(th, fiber);
    sb_fiber_list_unlock(th);
    sb_fiber_exit_pa(self);
}

/* Make FIBER, a main fiber registered with TH, or no fiber when NULL, the
 * one TH is running.  The Lisp side keeps its own current-fiber slot in
 * step. */
void sb_fiber_set_current(struct thread *th, struct sb_fiber_ctx *fiber)
{
    sb_fiber_enter_pa(th);
    thread_extra_data(th)->current_fiber = fiber;
    sb_fiber_exit_pa(th);
}

void sb_fiber_unregister(struct thread *th, struct sb_fiber_ctx *fiber)
{
    struct thread *self = get_sb_vm_thread();
    sb_fiber_enter_pa(self);
    sb_fiber_list_lock(th);
    if (thread_extra_data(th)->current_fiber == fiber)
        thread_extra_data(th)->current_fiber = NULL;
    if (fiber->owner == th && fiber_list_remove(th, fiber) == 0) {
        fiber->next = NULL;
        fiber->owner = NULL;
    }
    sb_fiber_list_unlock(th);
    sb_fiber_exit_pa(self);
}

/* Both list operations run holding the list owner's lock. */
static int fiber_list_remove(struct thread *src, struct sb_fiber_ctx *fiber)
{
    struct sb_fiber_ctx **pp = &thread_extra_data(src)->fiber_list;
    while (*pp && *pp != fiber) pp = &(*pp)->next;
    if (!*pp) return -1;               /* fiber not on this list */
    *pp = fiber->next;
    return 0;
}

static void fiber_list_insert(struct thread *dest, struct sb_fiber_ctx *fiber)
{
    struct extra_thread_data *ed = thread_extra_data(dest);
    fiber->next = ed->fiber_list;
    __atomic_store_n(&ed->fiber_list, fiber, __ATOMIC_RELEASE);
}

/* Callable from any thread, including one that is neither FIBER's owner
 * nor DEST.  It holds both lists' locks, taken in address order, so the
 * move is atomic to every other list operation.  The caller guarantees
 * that nothing resumes FIBER meanwhile. */
int sb_fiber_migrate(struct sb_fiber_ctx *fiber, struct thread *dest)
{
    if (fiber->state != FIBER_RUNNABLE) return -1;

    struct thread *self = get_sb_vm_thread();
    sb_fiber_enter_pa(self);
    struct thread *src;
    for (;;) {
        src = __atomic_load_n(&fiber->owner, __ATOMIC_ACQUIRE);
        if (!src || src == dest) break;
        struct thread *first  = src < dest ? src : dest;
        struct thread *second = src < dest ? dest : src;
        sb_fiber_list_lock(first);
        sb_fiber_list_lock(second);
        if (__atomic_load_n(&fiber->owner, __ATOMIC_ACQUIRE) == src) break;
        sb_fiber_list_unlock(second);
        sb_fiber_list_unlock(first);
    }
    int rc;
    if (!src) {
        rc = -2;
    } else if (src == dest) {
        rc = 0;
    } else {
        rc = fiber_list_remove(src, fiber);
        if (rc == 0) {
            sb_fiber_rebind_thread(fiber, dest);
            __atomic_store_n(&fiber->owner, dest, __ATOMIC_RELEASE);
            fiber_list_insert(dest, fiber);
        }
        sb_fiber_list_unlock(src);
        sb_fiber_list_unlock(dest);
    }
    sb_fiber_exit_pa(self);
    return rc;
}

static inline void swap_bindings_forward (struct thread *th,
                                          lispobj *base, lispobj *limit);
static inline void swap_bindings_backward(struct thread *th,
                                          lispobj *base, lispobj *limit);

void sb_fiber_lower_bs_guard(struct sb_fiber_ctx *f)
{
    size_t ps = os_reported_page_size;
    char *base = (char *)f->binding_stack_base;
    size_t usable = f->binding_stack_alloc_size - 3 * ps;
    mprotect(base + usable + ps, ps, PROT_READ | PROT_WRITE);
    mprotect(base + usable,      ps, PROT_NONE);
    f->bs_guard_protected = 0;
}

void sb_fiber_reset_bs_guard(struct sb_fiber_ctx *f)
{
    size_t ps = os_reported_page_size;
    char *base = (char *)f->binding_stack_base;
    size_t usable = f->binding_stack_alloc_size - 3 * ps;
    mprotect(base + usable + ps, ps, PROT_NONE);
    mprotect(base + usable,      ps, PROT_READ | PROT_WRITE);
    f->bs_guard_protected = 1;
}

static int sb_fiber_classify_bs_fault(struct thread *th, void *addr,
                                      struct sb_fiber_ctx **out_fiber)
{
    size_t ps = os_reported_page_size;
    char *a = (char *)addr;
    for (struct sb_fiber_ctx *f = thread_extra_data(th)->fiber_list;
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

int sb_fiber_handle_bs_fault(void *context_, void *addr, struct thread *th)
{
    os_context_t *context = (os_context_t *)context_;
    struct sb_fiber_ctx *f = NULL;
    /* The handler runs with the blockable signals blocked. */
    sb_fiber_list_lock(th);
    int kind = sb_fiber_classify_bs_fault(th, addr, &f);
    sb_fiber_list_unlock(th);
    if (kind == 1) {                    /* HARD guard hit */
        fake_foreign_function_call(context);
        lose("Fiber binding stack exhausted, fault: %p, PC: %p",
             addr, (void*)os_context_pc(context));
    } else if (kind == 2) {             /* SOFT guard hit */
        sb_fiber_lower_bs_guard(f);
        if (lose_on_corruption_p) {
            fake_foreign_function_call(context);
            lose("Fiber binding stack exhausted (lose-on-corruption)");
        }
        unblock_signals_in_context_and_maybe_warn(context);
        arrange_return_to_lisp_function
            (context, StaticSymbolFunction(BINDING_STACK_EXHAUSTED_ERROR));
        return 1;
    } else if (kind == 3) {             /* RETURN guard -- re-arm */
        sb_fiber_reset_bs_guard(f);
        return 1;
    }
    return 0;
}

typedef void (*fiber_swap_regs_fn)(void *save, void *restore);

/* A fiber can migrate to a different OS thread inside entry_fn.  The
 * compiler may cache the TLS-slot address of `current_thread', making
 * causing post-yield `get_sb_vm_thread()' to read the old thread's
 * slot.  Hide the read behind a noinline to defeat the cache. */
static __attribute__((noinline)) struct thread *fresh_sb_vm_thread(void)
{
    return get_sb_vm_thread();
}

void fiber_tramp_c(struct sb_fiber_ctx *self)
{
    struct thread *th = get_sb_vm_thread();
#ifdef LISP_FEATURE_ARM64
    th->control_stack_pointer = self->control_stack_pointer;
    th->control_frame_pointer = self->control_frame_pointer;
#endif
    sb_fiber_exit_pa(th);
    self->entry_fn(self->entry_arg);
    self->state = FIBER_DEAD;

    if (self->return_fiber) {
        struct sb_fiber_ctx *ret = self->return_fiber;
        struct thread *th = fresh_sb_vm_thread();
        sb_fiber_switch_prep(self, ret);
        th->current_catch_block          = ret->current_catch_block;
        th->current_unwind_protect_block = ret->current_unwind_protect_block;
        set_binding_stack_pointer(th, ret->binding_stack_pointer);
        th->binding_stack_start          = ret->binding_stack_start_for_thread;
        ret->state = FIBER_RUNNING;
        ((fiber_swap_regs_fn)SYMBOL(FIBER_SWAP_REGS)->value)
            (&self->regs, &ret->regs);
    }
    lose("fiber_tramp_c reached past auto-return");
}

/* --- Binding-stack swap --- */

#ifdef LISP_FEATURE_TLS_LOAD_INDIRECT
extern lispobj *tlsindex_to_symbol_map;
extern int tls_map_starting_offset;
#  ifndef NO_TLS_VALUE_MARKER
#    define NO_TLS_VALUE_MARKER (~(uword_t)0)
#  endif
#endif

static inline void exchange_binding_with_tls(struct thread *th, struct binding *b)
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
         * map-starting offset (others are :always-thread-local; see
         * README.md "Binding-stack swap"). */
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

/* Swap-in: oldest -> newest. */
static inline void swap_bindings_forward(struct thread *th,
                                         lispobj *base, lispobj *limit)
{
    for (struct binding *b = (struct binding *)base,
           *end = (struct binding *)limit;
         b < end; b++)
        exchange_binding_with_tls(th, b);
}

/* Swap-out: newest -> oldest. */
static inline void swap_bindings_backward(struct thread *th,
                                          lispobj *base, lispobj *limit)
{
    struct binding *start = (struct binding *)base;
    struct binding *b     = (struct binding *)limit;
    while (b > start) { b--; exchange_binding_with_tls(th, b); }
}

__attribute__((noinline))
void sb_fiber_switch_prep(struct sb_fiber_ctx *from, struct sb_fiber_ctx *to)
{
    struct thread *th = get_sb_vm_thread();
    sb_fiber_enter_pa(th);

    thread_extra_data(th)->current_fiber = to;

    from->binding_stack_pointer = get_binding_stack_pointer(th);

    sb_fiber_lisp_stack_suspend(from, th);
    sb_fiber_lisp_stack_resume(to, th);

    /* A fiber can be switched out inside an interrupt -- a handler of an
     * internal error that parks.  The context index travels with its
     * binding stack, but the contexts are per thread: save FROM's, and
     * give TO back the ones it left, or it and the GC read another
     * fiber's context through its index. */
    int from_contexts = fixnum_value(read_TLS(FREE_INTERRUPT_CONTEXT_INDEX, th));
    for (int i = 0; i < from_contexts; i++)
        from->sigcontexts[i] = nth_interrupt_context(i, th);
    from->n_sigcontexts = from_contexts;

    if (from->binding_stack_base)
        swap_bindings_backward(th, from->binding_stack_base,
                               from->binding_stack_pointer);
    if (to->binding_stack_base)
        swap_bindings_forward(th, to->binding_stack_base,
                              to->binding_stack_pointer);

    int to_contexts = fixnum_value(read_TLS(FREE_INTERRUPT_CONTEXT_INDEX, th));
    for (int i = 0; i < to_contexts && i < to->n_sigcontexts; i++)
        nth_interrupt_context(i, th) = to->sigcontexts[i];
    for (int i = to_contexts; i < from_contexts; i++)
        nth_interrupt_context(i, th) = NULL;
}


static inline void sb_fiber_enter_pa(struct thread *th)
{
#if defined LISP_FEATURE_ARM64
    ((volatile uint32_t *)&th->pseudo_atomic_bits)[0] = flag_PseudoAtomic;
#else
    th->pseudo_atomic_bits = (uword_t)th;
#endif
}

void sb_fiber_exit_pa(struct thread *th)
{
#if defined LISP_FEATURE_ARM64
    volatile uint32_t *halves = (volatile uint32_t *)&th->pseudo_atomic_bits;
    halves[0] = 0;
    if (halves[1]) {
        halves[1] = 0;
        asm volatile("brk %0" : : "i"(trap_PendingInterrupt));
    }
#else
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
