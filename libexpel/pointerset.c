/*
 * pointerset.c: ordered sets of pointers
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


#include <strings.h>
#include <string.h>
#include <stdlib.h>

#include "expel/pointerset.h"

xl_error
_grow_pointer_set(struct xl_pointer_set *set)
{
        size_t new_cap;
        void **new_elems;

        if (set->cap == 0)
        {
                set->elems = calloc(8, sizeof(void *));
                if (set->elems == NULL)
                        return xl_raise(ERR_NO_MEMORY, "pointer set alloc");
                set->cap = 8;
                return OK;
        }

        new_cap = set->cap * 2;
        new_elems = realloc(set->elems, new_cap * sizeof(void *));
        if (new_elems == NULL)
                return xl_raise(ERR_NO_MEMORY, "pointer set alloc");

        /* Zero out the new elements (if only there were a crealloc */
        bzero(&set->elems[set->cap], new_cap - set->cap);

        set->cap = new_cap;
        return OK;
}

xl_error
xl_pointer_set_add(bool *added, struct xl_pointer_set *set, void *item)
{
        size_t start;
        size_t end;
        size_t check;
        uintptr_t t;
        uintptr_t ins;
        xl_error err;

        ins = (uintptr_t) item;
        start = 0;
        end = set->n;

        /* Find the insertion point by doing a binary traversal of the array.
         * When this process is complete, start == end and the index they point
         * to is the index we want to insert our value at. */
        while (end > start)
        {
                check = (start + end) / 2;
                t = (uintptr_t) set->elems[check];
                if (t == ins)
                {
                        *added = false;
                        return OK;
                }
                if (t < ins)
                        start = check + 1;
                else
                        end = check;
        }

        if (set->n == set->cap)
        {
                err = _grow_pointer_set(set);
                if (err != OK)
                        return err;
        }

        /* Shift everything over by one to insert the new item. */
        memmove(&set->elems[start+1], &set->elems[start], set->n - start);

        set->elems[start] = item;
        *added = true;
        return OK;
}

xl_error
xl_pointer_set_present(bool *present, struct xl_pointer_set *, void *item);

xl_error
xl_pointer_set_find(size_t *index, struct xl_pointer_set *, void *item);
