/*
 * Page provenance for link cores; see corefrag.h.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <time.h>
#include <sys/types.h>
#include <unistd.h>

#include "genesis/sbcl.h"
#include "runtime.h"
#include "os.h"
#include "core.h"
#include "corefrag.h"
#include "interr.h"

struct corefrag_source *corefrag_sources;
int corefrag_n_sources;
struct corefrag_run *corefrag_runs;
int corefrag_n_runs;
static int sources_capacity, runs_capacity;

int corefrag_add_source(char *path, uword_t *id, os_vm_offset_t core_start)
{
    if (corefrag_n_sources == sources_capacity) {
        sources_capacity = sources_capacity ? 2*sources_capacity : 4;
        corefrag_sources = realloc(corefrag_sources,
                                   sources_capacity * sizeof (struct corefrag_source));
        if (!corefrag_sources) lose("corefrag_add_source: out of memory");
    }
    struct corefrag_source *s = &corefrag_sources[corefrag_n_sources];
    s->path = path;
    memcpy(s->id, id, sizeof s->id);
    s->core_start = core_start;
    return corefrag_n_sources++;
}

void corefrag_add_run(uword_t addr, uword_t len, int source, os_vm_offset_t offset)
{
    if (!len) return;
    if (corefrag_n_runs) { // extend the previous run when this one continues it
        struct corefrag_run *prev = &corefrag_runs[corefrag_n_runs-1];
        if (prev->source == source && prev->addr + prev->len == addr
            && prev->offset + (os_vm_offset_t)prev->len == offset) {
            prev->len += len;
            return;
        }
    }
    if (corefrag_n_runs == runs_capacity) {
        runs_capacity = runs_capacity ? 2*runs_capacity : 64;
        corefrag_runs = realloc(corefrag_runs, runs_capacity * sizeof (struct corefrag_run));
        if (!corefrag_runs) lose("corefrag_add_run: out of memory");
    }
    struct corefrag_run *r = &corefrag_runs[corefrag_n_runs++];
    r->addr = addr;
    r->len = len;
    r->source = source;
    r->offset = offset;
}

/* Store in ID the CORE-ID entry of the core that starts at CORE_START in FD.
 * Return 0 if it has none. FD's file position is unchanged. */
int corefrag_read_core_id(int fd, os_vm_offset_t core_start, uword_t *id)
{
    os_vm_offset_t old = lseek(fd, 0, SEEK_CUR);
    core_entry_elt_t *header = calloc(os_vm_page_size, 1);
    int found = 0;
    if (header && lseek(fd, core_start, SEEK_SET) == core_start
        && read(fd, header, os_vm_page_size) == (ssize_t)os_vm_page_size
        && header[0] == CORE_MAGIC) {
        core_entry_elt_t *ptr = header + 1, *limit = header + os_vm_page_size/N_WORD_BYTES;
        while (ptr + 2 <= limit) {
            core_entry_elt_t type = ptr[0], len = ptr[1];
            if (type == END_CORE_ENTRY_TYPE_CODE || len < 2) break;
            if (type == CORE_ID_CORE_ENTRY_TYPE_CODE && len == 2 + COREFRAG_ID_WORDS) {
                memcpy(id, ptr + 2, COREFRAG_ID_WORDS * N_WORD_BYTES);
                found = 1;
                break;
            }
            ptr += len;
        }
    }
    free(header);
    lseek(fd, old, SEEK_SET);
    return found;
}

/* A fresh identifier for a core being saved. It need only differ from the
 * identifier of any other core a link core could be pointed at. */
void corefrag_new_id(uword_t *id)
{
    int got = 0;
#ifndef LISP_FEATURE_WIN32
    int fd = open("/dev/urandom", O_RDONLY);
    if (fd >= 0) {
        got = read(fd, id, COREFRAG_ID_WORDS * N_WORD_BYTES)
            == (ssize_t)(COREFRAG_ID_WORDS * N_WORD_BYTES);
        close(fd);
    }
#endif
    if (!got) {
        id[0] = (uword_t)time(NULL);
        id[1] = (uword_t)(uintptr_t)&got ^ (uword_t)clock();
    }
}

/* For Lisp, which decides with these whether to save a link core and which
 * files the core it saves may depend on. */
int corefrag_link_saves_supported = COREFRAG_LINK_SAVES;

/* The path of the Nth core this process loaded pages from, or NULL. */
char *corefrag_source_path(int n)
{
    return (n >= 0 && n < corefrag_n_sources) ? corefrag_sources[n].path : NULL;
}
