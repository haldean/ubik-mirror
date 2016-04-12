/*
 * pointer_set.c: run pointerset tests
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

#include <stdlib.h>

#include "ubik/ubik.h"
#include "ubik/pointerset.h"
#include "ubik/vector.h"
#include "unit.h"


test_t
pointer_set()
{
        local(vector) struct xl_vector s = {0};
        bool present;
        bool added;
        size_t index;
        xl_error err;
        void *t0, *t1, *t2, *t3, *t4;

        t0 = (void *)((uintptr_t) 24);
        t1 = (void *)((uintptr_t) 23);
        t2 = (void *)((uintptr_t) 1);
        t3 = (void *)((uintptr_t) 0xFFFFF);
        t4 = (void *)((uintptr_t) 0);

        assert(xl_pointer_set_present(&present, &s, t0) == OK);
        assert(!present);

        index = 17;
        err = xl_pointer_set_find(&index, &s, t0);
        assert(err != NULL && err->error_code == ERR_ABSENT);
        free(err);
        assert(index == 17);

        assert(xl_pointer_set_add(&added, &s, t0) == OK);
        assert(added);

        assert(xl_pointer_set_present(&present, &s, t0) == OK);
        assert(present);
        assert(xl_pointer_set_find(&index, &s, t0) == OK);
        assert(index == 0);

        assert(xl_pointer_set_present(&present, &s, t1) == OK);
        assert(!present);
        err = xl_pointer_set_find(&index, &s, t1);
        assert(err != NULL && err->error_code == ERR_ABSENT);
        free(err);

        assert(s.elems[0] == t0);

        assert(xl_pointer_set_add(&added, &s, t0) == OK);
        assert(!added);

        assert(xl_pointer_set_add(&added, &s, t1) == OK);
        assert(added);
        assert(s.elems[0] == t1);
        assert(s.elems[1] == t0);

        assert(xl_pointer_set_add(&added, &s, t2) == OK);
        assert(added);
        assert(s.elems[0] == t2);
        assert(s.elems[1] == t1);
        assert(s.elems[2] == t0);

        assert(xl_pointer_set_add(&added, &s, t3) == OK);
        assert(added);
        assert(s.elems[0] == t2);
        assert(s.elems[1] == t1);
        assert(s.elems[2] == t0);
        assert(s.elems[3] == t3);

        assert(xl_pointer_set_add(&added, &s, t4) == OK);
        assert(added);
        assert(xl_pointer_set_add(&added, &s, t4) == OK);
        assert(!added);

        assert(s.elems[0] == t4);
        assert(s.elems[1] == t2);
        assert(s.elems[2] == t1);
        assert(s.elems[3] == t0);
        assert(s.elems[4] == t3);

        return ok;
}

run_single(pointer_set)
