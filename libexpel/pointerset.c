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

no_ignore static xl_error
_pointer_set_grow(struct xl_pointer_set *set)
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
        set->elems = new_elems;

        /* Zero out the new elements (if only there were a crealloc */
        bzero(&set->elems[set->cap], new_cap - set->cap);

        set->cap = new_cap;
        return OK;
}

/* Finds the logical index of the given item, even if the item is not itself
 * present. If the item is not present, the returned index is the index at which
 * the item should be inserted. */
no_ignore static xl_error
_pointer_set_index(size_t *index, struct xl_pointer_set *set, void *item)
{
        size_t start;
        size_t end;
        size_t check;
        uintptr_t t;
        uintptr_t ins;

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
                        *index = check;
                        return OK;
                }
                if (t < ins)
                        start = check + 1;
                else
                        end = check;
        }

        *index = start;
        return OK;
}

no_ignore xl_error
xl_pointer_set_add(bool *added, struct xl_pointer_set *set, void *item)
{
        size_t insert_at;
        xl_error err;

        err = _pointer_set_index(&insert_at, set, item);
        if (err != OK)
                return err;
        if (set->elems != NULL
                && insert_at < set->n
                && set->elems[insert_at] == item)
        {
                if (added != NULL)
                        *added = false;
                return OK;
        }

        if (set->n == set->cap)
        {
                err = _pointer_set_grow(set);
                if (err != OK)
                        return err;
        }

        /* Shift everything over by one to insert the new item. */
        memmove(&set->elems[insert_at+1],
                &set->elems[insert_at],
                sizeof(void *) * (set->n - insert_at));

        set->elems[insert_at] = item;
        if (added != NULL)
                *added = true;
        set->n += 1;
        return OK;
}

no_ignore xl_error
xl_pointer_set_present(bool *present, struct xl_pointer_set *set, void *item)
{
        size_t index;
        xl_error err;

        if (set->cap == 0)
        {
                *present = false;
                return OK;
        }

        err = _pointer_set_index(&index, set, item);
        if (err != OK)
                return err;
        *present = set->elems[index] == item && index < set->n;
        return OK;
}

no_ignore xl_error
xl_pointer_set_find(size_t *ret_index, struct xl_pointer_set *set, void *item)
{
        size_t index;
        xl_error err;

        if (set->cap == 0)
                return xl_raise(ERR_ABSENT, "pointer set find");

        err = _pointer_set_index(&index, set, item);
        if (err != OK)
                return err;
        if (index >= set->n || set->elems[index] != item)
                return xl_raise(ERR_ABSENT, "pointer set find");
        *ret_index = index;
        return OK;
}
