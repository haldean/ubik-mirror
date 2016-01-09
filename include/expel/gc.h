/*
 * gc.h: garbage collector introspection
 * Copyright (C) 2016, Haldean Brown
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include "expel/expel.h"

/* The number of values in a page. */
#define XL_GC_PAGE_SIZE 8
/* The number of calls to xl_release before we start a GC sweep. */
#define XL_GC_TRIGGER_RELEASES (4 * XL_GC_PAGE_SIZE)

struct xl_alloc_page
{
        /* Pages are stored in a linked list. */
        struct xl_alloc_page *prev;
        struct xl_alloc_page *next;

        /* A pointer to the block in memory that this page represents. */
        struct xl_value *values;

        /* The free list of values that are available. This starts with a
         * pointer to every element in the page, and as spots are used these
         * pointers are popped off. When a value is freed, its pointer is added
         * back onto this array. */
        struct xl_value *open_values[XL_GC_PAGE_SIZE];

        /* The number of valid open values in the open_values array. */
        int64_t n_open_values;
};

struct xl_gc_info
{
        int64_t releases_until_gc;

#ifdef XL_GC_DEBUG
        uint64_t n_page_allocs;
        uint64_t n_page_frees;
        uint64_t n_val_allocs;
        uint64_t n_val_frees;
        uint64_t n_gc_runs;
        uint64_t n_graph_frees;
#endif
};

/* Initialize the garbage collector.
 *
 * This must be called before xl_new can be called. Calling this
 * drops all existing GC state, and thus don't call it more than
 * once unless you /really/ know what you're doing. */
void
xl_gc_start();

/* Teardown the garbage collector.
 *
 * Frees all memory known to the garbage collector. After calling
 * this, ou must call xl_gc_start again if you want to use the
 * runtime again. */
void
xl_gc_teardown();

/* Get garbage collector stats. */
void
xl_gc_get_stats(struct xl_gc_info *stats);

/* Free everything the garbage collector knows about.
 *
 * This will seriously fuck with the runtime, because pretty much
 * every pointer becomes invalid. You have been warned. */
void
xl_gc_free_all();

/* Runs garbage collection and removes empty pages.
 *
 * There should be no reason to call this; xl_release calls it
 * when appropriate.  This is provided as an API for garbage
 * collection testing only. */
no_ignore xl_error_t
xl_run_gc();

