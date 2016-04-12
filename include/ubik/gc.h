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

#include "ubik/expel.h"

struct xl_gc_info
{
#ifdef XL_GC_DEBUG
        uint64_t n_val_allocs;
        uint64_t n_val_frees;
        uint64_t n_graph_allocs;
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

