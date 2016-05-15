/*
 * list.c: run tests on lists
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

#include "ubik/ubik.h"
#include "ubik/list.h"
#include "unit.h"

test_t
list()
{
        struct ubik_value *lst;
        struct ubik_value *v;
        size_t i;

        assert(ubik_value_new(&lst) == OK);
        assert(ubik_list_create_empty(lst) == OK);

        assert(ubik_list_size(&i, lst) == OK);
        assert(i == 0);

        for (i = 0; i < 10; i++)
        {
                assert(ubik_value_new(&v) == OK);
                v->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
                v->left.w = i;
                v->right.w = 14;
                assert(ubik_list_append(lst, v) == OK);
                assert(ubik_release(v) == OK);
                assert(v->refcount == 1);
        }

        for (i = 0; i < 10; i++)
        {
                assert(ubik_list_get(&v, lst, i) == OK);
                assert(v->left.w == i);
                assert(v->right.w == 14);
        }

        assert(ubik_release(lst) == OK);
        return ok;
}

run_single(list)
