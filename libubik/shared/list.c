/*
 * list.h: heterogenous value-encoded lists
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

#include "ubik/alloc.h"
#include "ubik/list.h"
#include "ubik/util.h"

/* Creates an empty list backed by the given value. */
no_ignore ubik_error
ubik_list_create_empty(struct ubik_value *lst)
{
        lst->type = UBIK_TUP;
        lst->tup.n = 0;
        return OK;
}

/* Appends an item onto the end of the list. */
no_ignore ubik_error
ubik_list_append(
        struct ubik_value *lst,
        struct ubik_value *v,
        struct ubik_workspace *ws)
{
        ubik_error err;

        if (lst->type != UBIK_TUP || (lst->tup.n != 2 && lst->tup.n != 0))
                return ubik_raise(
                        ERR_BAD_TAG, "provided value is not a list");
        while (lst->tup.n == 2)
        {
                lst = lst->tup.elems[1];
                if (unlikely(lst->type != UBIK_TUP))
                        return ubik_raise(
                                ERR_BAD_TAG, "provided value is not a list");
        }

        lst->tup.n = 2;
        ubik_galloc((void**) &lst->tup.elems, 2, sizeof(struct ubik_value *));
        ubik_galloc((void**) &lst->tup.types, 2, sizeof(struct ubik_value *));

        lst->tup.elems[0] = v;

        err = ubik_value_new(&lst->tup.elems[1], ws);
        if (err != OK)
                return err;
        lst->tup.elems[1]->type = UBIK_TUP;

        return OK;
}

no_ignore ubik_error
ubik_list_extend(struct ubik_value *lst, struct ubik_value *ex)
{
        size_t size;
        ubik_error err;

        err = ubik_list_size(&size, ex);
        if (err != OK)
                return err;

        if (size == 0)
                return OK;

        err = ubik_list_size(&size, lst);
        if (err != OK)
                return err;

        if (size == 0)
        {
                *lst = *ex;
                return OK;
        }

        while (lst->tup.n == 2 && lst->tup.elems[1]->tup.n == 2)
        {
                lst = lst->tup.elems[1];
                if (unlikely(lst->type != UBIK_TUP))
                        return ubik_raise(
                                ERR_BAD_TAG, "provided value is not a list");
        }
        lst->tup.elems[1] = ex;
        return OK;
}

/* Returns the number of items in the list. */
no_ignore ubik_error
ubik_list_size(size_t *ret_size, struct ubik_value *lst)
{
        size_t size;

        size = 0;
        for (size = 0; lst->tup.n == 2; size++, lst = lst->tup.elems[1])
        {
                if (unlikely(lst->type != UBIK_TUP || lst->tup.n != 2))
                        return ubik_raise(
                                ERR_BAD_TAG, "provided value is not a list");
        }

        *ret_size = size;
        return OK;
}

/* Returns the item at the given index in the list. */
no_ignore ubik_error
ubik_list_get(
        struct ubik_value **val,
        struct ubik_value *lst,
        size_t i)
{
        size_t seen;

        for (seen = 0;
             seen < i && lst->tup.n == 2;
             seen++, lst = lst->tup.elems[1])
        {
                if (unlikely(lst->type != UBIK_TUP || lst->tup.n != 2))
                        return ubik_raise(
                                ERR_BAD_TAG, "provided value is not a list");
        }

        if (seen != i)
                return ubik_raise(
                        ERR_OUT_OF_BOUNDS,
                        "index out of bounds in list access");
        *val = lst->tup.elems[0];
        return OK;
}
