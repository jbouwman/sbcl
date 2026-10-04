/*
 * Activating a fragment file: the runtime half.
 *
 * A fragment file (tools-for-build/corefrag-writer.lisp) holds page runs
 * laid out at planned addresses. Activation for the prebound case, where
 * this process runs the core the fragment was written against and the
 * planned pages are free, maps each run from the file at its address,
 * installs its allocation bits and page table entries as a core's pages
 * are installed at boot, and accounts the bytes to the pseudo-static
 * generation. Relocation of a run whose pages are taken, resolution of
 * imports by name, and the replay of records are the Lisp half's, not
 * done here.
 */

#include "genesis/sbcl.h"
#ifdef LISP_FEATURE_MARK_REGION_GC

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include "runtime.h"
#include "os.h"
#include "gc.h"
#include "interr.h"
#include "validate.h"
#include "corefrag.h"

#define FRAG_MAGIC 0x53424652
#define FRAG_VERSION 1
#define FRAG_SECTION_PAGES 2
#define FRAG_ALLOC_UNIT (2*N_WORD_BYTES)

struct frag_run {
    uword_t type, base, nbytes, npages, nobjects;
    uword_t *ptes;              /* 2 words per page: words used, scan start offset */
    os_vm_offset_t bitmap_offset, bytes_offset; /* in the file */
};

static int read_words(int fd, os_vm_offset_t offset, uword_t *words, int n)
{
    if (lseek(fd, offset, SEEK_SET) != offset) return -1;
    return read(fd, words, n * sizeof (uword_t)) == (ssize_t)(n * sizeof (uword_t)) ? 0 : -1;
}

static uword_t round_to_page(uword_t n) { return ALIGN_UP(n, GENCGC_PAGE_BYTES); }

/* Activate the fragment at PATH. Returns 0, or a negative code:
 * -1 unreadable or not a fragment file, -2 unsupported version,
 * -3 a run outside dynamic space or not page-aligned, -4 a planned page
 * in use, -5 mapping failed. OUT receives the number of runs and the
 * bytes mapped. */
int corefrag_activate_file(char *path, uword_t *out)
{
    int fd = open(path, O_RDONLY);
    if (fd < 0) return -1;
    uword_t head[3];
    if (read_words(fd, 0, head, 3) || head[0] != FRAG_MAGIC) { close(fd); return -1; }
    if (head[1] != FRAG_VERSION) { close(fd); return -2; }
    int nsections = (int)head[2];
    os_vm_offset_t pages_offset = -1; uword_t pages_length = 0;
    for (int i = 0; i < nsections; i++) {
        uword_t entry[3];
        if (read_words(fd, (3 + 3*i) * sizeof (uword_t), entry, 3)) { close(fd); return -1; }
        if (entry[0] == FRAG_SECTION_PAGES) { pages_offset = entry[1]; pages_length = entry[2]; }
    }
    if (pages_offset < 0) { close(fd); return -1; }

    /* The section's header: the run count, then per run five words and
     * two per page. */
    uword_t nruns;
    if (read_words(fd, pages_offset, &nruns, 1)) { close(fd); return -1; }
    struct frag_run *runs = calloc(nruns, sizeof *runs);
    os_vm_offset_t cursor = pages_offset + sizeof (uword_t);
    uword_t header_words = 1;
    for (uword_t r = 0; r < nruns; r++) {
        uword_t fields[5];
        if (read_words(fd, cursor, fields, 5)) goto bad;
        cursor += 5 * sizeof (uword_t); header_words += 5;
        runs[r].type = fields[0]; runs[r].base = fields[1]; runs[r].nbytes = fields[2];
        runs[r].npages = fields[3]; runs[r].nobjects = fields[4];
        runs[r].ptes = calloc(2 * runs[r].npages, sizeof (uword_t));
        if (read_words(fd, cursor, runs[r].ptes, 2 * runs[r].npages)) goto bad;
        cursor += 2 * runs[r].npages * sizeof (uword_t); header_words += 2 * runs[r].npages;
    }
    /* Then, page-aligned: for each run its allocation bitmap padded to a
     * page, then its bytes. */
    os_vm_offset_t data = pages_offset + round_to_page(header_words * sizeof (uword_t));
    for (uword_t r = 0; r < nruns; r++) {
        uword_t bitmap_bytes = (runs[r].nbytes / FRAG_ALLOC_UNIT + 7) / 8;
        runs[r].bitmap_offset = data;
        data += round_to_page(bitmap_bytes);
        runs[r].bytes_offset = data;
        data += runs[r].nbytes;
    }
    if ((uword_t)(data - pages_offset) > round_to_page(pages_length)) goto bad;

    /* Every run must lie in dynamic space on free pages. */
    for (uword_t r = 0; r < nruns; r++) {
        uword_t base = runs[r].base, end = base + runs[r].nbytes;
        if (base % GENCGC_PAGE_BYTES || runs[r].nbytes % GENCGC_PAGE_BYTES
            || base < DYNAMIC_SPACE_START || end > DYNAMIC_SPACE_START + dynamic_space_size) {
            free(runs); close(fd); return -3;
        }
        for (page_index_t p = find_page_index((void*)base); p < find_page_index((void*)end); p++)
            if (!page_free_p(p)) { free(runs); close(fd); return -4; }
    }

    uword_t total = 0;
    acquire_gc_page_table_lock();
    for (uword_t r = 0; r < nruns; r++) {
        struct frag_run *run = &runs[r];
        if (!load_core_bytes(fd, run->bytes_offset, (os_vm_address_t)run->base, run->nbytes, 0)) {
            release_gc_page_table_lock(); free(runs); close(fd); return -5;
        }
        /* The allocation bitmap: one bit per unit, a whole number of bytes
         * per page, so the run's bytes land at the page's offset. */
        page_index_t first = find_page_index((void*)run->base);
        uword_t bitmap_bytes = (run->nbytes / FRAG_ALLOC_UNIT + 7) / 8;
        unsigned char *bits = (unsigned char*)allocation_bitmap + first * (GENCGC_PAGE_BYTES / FRAG_ALLOC_UNIT / 8);
        if (lseek(fd, run->bitmap_offset, SEEK_SET) != run->bitmap_offset
            || read(fd, bits, bitmap_bytes) != (ssize_t)bitmap_bytes) {
            release_gc_page_table_lock(); free(runs); close(fd); return -1;
        }
        generation_index_t gen = PSEUDO_STATIC_GENERATION;
        for (uword_t p = 0; p < run->npages; p++) {
            page_index_t page = first + p;
            uword_t words = run->ptes[2*p], sso = run->ptes[2*p+1];
            char type = ((words & 1) ? SINGLE_OBJECT_FLAG : 0) | (sso & 0x07);
            words >>= 1;
            page_table[page].type = type;
            if (!words) { page_table[page].type = FREE_PAGE_FLAG; continue; }
            page_table[page].words_used_ = words;
            set_page_scan_start_offset(page, sso & ~0x07);
            page_table[page].gen = gen;
            if (!page_single_obj_p(page))
                for_lines_in_page(l, page) line_bytemap[l] = ENCODE_GEN(gen);
            bytes_allocated += words << WORD_SHIFT;
            generations[gen].bytes_allocated += words << WORD_SHIFT;
        }
        if (first + (page_index_t)run->npages > next_free_page)
            next_free_page = first + run->npages;
        total += run->nbytes;
    }
    release_gc_page_table_lock();
    for (uword_t r = 0; r < nruns; r++) free(runs[r].ptes);
    free(runs); close(fd);
    if (out) { out[0] = nruns; out[1] = total; }
    return 0;
bad:
    for (uword_t r = 0; r < nruns; r++) free(runs[r].ptes);
    free(runs); close(fd); return -1;
}

#endif /* LISP_FEATURE_MARK_REGION_GC */
