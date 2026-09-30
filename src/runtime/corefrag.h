/*
 * Where the pages of this process's spaces came from.
 *
 * Loading a core records, for each run of pages it maps or reads, the
 * core file the bytes came from and where in that file they are. A link
 * save (save-lisp-and-die :link t) writes only the pages that differ from
 * those bytes and refers to the source files for the rest. A space read
 * from compressed data records nothing, so a link save writes all of it.
 */

#ifndef _COREFRAG_H_
#define _COREFRAG_H_

#include "core.h"
#include "os.h"

#define COREFRAG_ID_WORDS 2
/* A link core refers to the files that hold its pages, not to the cores those
 * files were themselves saved against, so a chain of saves adds one file per
 * layer and no more. */
#define MAX_CORE_PARENTS 16

/* The data page of a link core's directory entry, whose pages are in runs. */
#define LINK_CORE_DATA_PAGE (-1)

struct corefrag_source {
    char *path;                         /* absolute */
    uword_t id[COREFRAG_ID_WORDS];      /* the CORE-ID entry of the core at PATH */
    os_vm_offset_t core_start;          /* byte offset of that core in the file */
};

struct corefrag_run {
    uword_t addr;                       /* first byte in memory */
    uword_t len;                        /* a multiple of os_vm_page_size */
    int source;                         /* index into corefrag_sources */
    os_vm_offset_t offset;              /* byte offset of ADDR's page in the source file */
};

/* A run as a link core stores it, in the blob a SPACE-RUNS entry names.
 * SOURCE 0 is the link core itself, and N the Nth of its PARENT-CORES. */
struct corefrag_stored_run {
    core_entry_elt_t space_id;
    core_entry_elt_t space_offset;      /* bytes from the start of the space */
    core_entry_elt_t len;               /* bytes */
    core_entry_elt_t source;
    core_entry_elt_t data_page;         /* as a directory entry's, relative to the source core */
};
#define COREFRAG_STORED_RUN_WORDS (sizeof (struct corefrag_stored_run)/sizeof (core_entry_elt_t))

extern struct corefrag_source *corefrag_sources;
extern int corefrag_n_sources;
extern struct corefrag_run *corefrag_runs;
extern int corefrag_n_runs;

extern int corefrag_add_source(char *path, uword_t *id, os_vm_offset_t core_start);
extern void corefrag_add_run(uword_t addr, uword_t len, int source, os_vm_offset_t offset);
extern int corefrag_read_core_id(int fd, os_vm_offset_t core_start, uword_t *id);
extern void corefrag_new_id(uword_t *id);

#endif
