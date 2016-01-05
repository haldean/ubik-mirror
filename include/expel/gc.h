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

#define PAGE_SIZE 256
#define TRIGGER_GC_ON_FREES (4 * PAGE_SIZE)

struct xl_alloc_page
{
        struct xl_alloc_page *prev;
        struct xl_alloc_page *next;
        struct xl_value values[PAGE_SIZE];
        uint16_t used;
};

struct xl_gc_stats
{
        uint32_t n_release_since_gc;

#ifdef XL_DEBUG_GC
        uint64_t n_page_allocs;
        uint64_t n_page_frees;
        uint64_t n_val_allocs;
        uint64_t n_val_frees;
        uint64_t n_gc_runs;
#endif
};

/* Initialize the garbage collector.
 *
 * This must be called before xl_new can be called. Calling this
 * drops all existing GC state, and thus don't call it more than
 * once unless you /really/ know what you're doing. */
void
xl_gc_start();

/* Runs garbage collection and removes empty pages.
 *
 * There should be no reason to call this; xl_release calls it
 * when appropriate.  This is provided as an API for garbage
 * collection testing only. */
no_ignore word_t
run_gc();

