/*
 * gc.c: run garbage collector tests
 * Copyright (C) 2015, Haldean Brown
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

#include "ubik/ubik.h"
#include "ubik/gc.h"
#include "unit.h"


test_t
gc()
{
        #define N_TEST_GC_VALUES 10000

        size_t i;
        struct xl_value *vals[N_TEST_GC_VALUES];
        struct xl_gc_info gc_stats;

        xl_gc_free_all();
        xl_gc_start();

        for (i = 0; i < N_TEST_GC_VALUES; i++)
        {
                assert(xl_value_new(&vals[i]) == 0);
        }
        for (i = 0; i < N_TEST_GC_VALUES; i++)
        {
                assert(xl_release(vals[i]) == 0);
        }

        xl_gc_get_stats(&gc_stats);
        #ifdef XL_GC_DEBUG
                assert(gc_stats.n_val_allocs == N_TEST_GC_VALUES);
                assert(gc_stats.n_val_frees == N_TEST_GC_VALUES);
        #endif

        xl_gc_free_all();
        xl_gc_start();

        return ok;
}

/* this has its own main function (instead of run_single) because we don't want
 * to start up the interpreter. */
int
main()
{
        init();
        run(gc);
        finish();
}
