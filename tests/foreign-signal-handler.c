/* C helper library that simulates the behavior of LLVM/libclang which
 * installs its own SIGSEGV handler, creates internal worker threads,
 * and expects its handler to remain installed for those threads
 */

#define _GNU_SOURCE
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include <stdatomic.h>
#include <sys/mman.h>

static struct sigaction saved_old_handler;
static volatile sig_atomic_t foreign_handler_called = 0;
static atomic_int thread_result = 0;
static atomic_int thread_may_proceed = 0;
static pthread_t worker_thread;

static void *fault_page = NULL;
static size_t fault_page_size = 0;

/* A SIGSEGV handler that simulates what LLVM installs.  When
 * triggered by the fault test, it recovers by making the faulting
 * page readable again. */
static void foreign_sigsegv_handler(int sig, siginfo_t *info, void *ctx)
{
    foreign_handler_called = 1;

    if (fault_page && info && info->si_addr >= fault_page
        && (char*)info->si_addr < (char*)fault_page + fault_page_size) {
        mprotect(fault_page, fault_page_size, PROT_READ | PROT_WRITE);
        return;  /* resume execution - the faulting instruction will retry */
    }

    if (saved_old_handler.sa_flags & SA_SIGINFO) {
        if (saved_old_handler.sa_sigaction &&
            saved_old_handler.sa_sigaction != (void*)SIG_DFL &&
            saved_old_handler.sa_sigaction != (void*)SIG_IGN) {
            saved_old_handler.sa_sigaction(sig, info, ctx);
        }
    } else {
        if (saved_old_handler.sa_handler &&
            saved_old_handler.sa_handler != SIG_DFL &&
            saved_old_handler.sa_handler != SIG_IGN) {
            saved_old_handler.sa_handler(sig);
        }
    }
}

int foreign_install_sigsegv_handler(void)
{
    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_sigaction = foreign_sigsegv_handler;
    sa.sa_flags = SA_SIGINFO | SA_RESTART;
    sigemptyset(&sa.sa_mask);

    if (sigaction(SIGSEGV, &sa, &saved_old_handler) != 0) {
        perror("foreign_install_sigsegv_handler: sigaction");
        return -1;
    }
    return 0;
}

unsigned long foreign_get_sigsegv_handler(void)
{
    struct sigaction cur;
    sigaction(SIGSEGV, NULL, &cur);
    return (unsigned long)cur.sa_sigaction;
}

unsigned long foreign_get_our_handler_addr(void)
{
    return (unsigned long)foreign_sigsegv_handler;
}

int foreign_handler_was_called(void)
{
    return foreign_handler_called;
}

void foreign_reset_handler_flag(void)
{
    foreign_handler_called = 0;
}

static void *worker_thread_check_handler(void *arg)
{
    (void)arg;

    while (!atomic_load(&thread_may_proceed)) {
        usleep(100);
    }

    unsigned long current_handler = foreign_get_sigsegv_handler();
    unsigned long our_handler = foreign_get_our_handler_addr();

    if (current_handler == our_handler) {
        atomic_store(&thread_result, 1);
    } else if (current_handler == (unsigned long)SIG_DFL ||
               current_handler == (unsigned long)SIG_IGN) {
        atomic_store(&thread_result, -1);
    } else {
        atomic_store(&thread_result, -2);
    }

    return NULL;
}

static void *worker_thread_trigger_fault(void *arg)
{
    (void)arg;

    while (!atomic_load(&thread_may_proceed)) {
        usleep(100);
    }

    fault_page_size = (size_t)sysconf(_SC_PAGESIZE);
    fault_page = mmap(NULL, fault_page_size, PROT_READ | PROT_WRITE,
                      MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (fault_page == MAP_FAILED) {
        fault_page = NULL;
        atomic_store(&thread_result, -1);
        return NULL;
    }

    memset(fault_page, 0x42, fault_page_size);

    if (mprotect(fault_page, fault_page_size, PROT_NONE) != 0) {
        munmap(fault_page, fault_page_size);
        fault_page = NULL;
        atomic_store(&thread_result, -1);
        return NULL;
    }

    volatile char val = ((volatile char *)fault_page)[0];
    (void)val;

    munmap(fault_page, fault_page_size);
    fault_page = NULL;
    atomic_store(&thread_result, 1);
    return NULL;
}

int foreign_spawn_worker_thread(void)
{
    atomic_store(&thread_may_proceed, 0);
    atomic_store(&thread_result, 0);
    if (pthread_create(&worker_thread, NULL, worker_thread_check_handler, NULL) != 0) {
        perror("foreign_spawn_worker_thread: pthread_create");
        return -1;
    }
    return 0;
}

int foreign_spawn_fault_thread(void)
{
    atomic_store(&thread_may_proceed, 0);
    atomic_store(&thread_result, 0);
    foreign_handler_called = 0;
    if (pthread_create(&worker_thread, NULL, worker_thread_trigger_fault, NULL) != 0) {
        perror("foreign_spawn_fault_thread: pthread_create");
        return -1;
    }
    return 0;
}

void foreign_release_worker_thread(void)
{
    atomic_store(&thread_may_proceed, 1);
}

int foreign_join_worker_thread(void)
{
    pthread_join(worker_thread, NULL);
    return atomic_load(&thread_result);
}
