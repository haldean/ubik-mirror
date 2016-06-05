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

#include "ubik/list.h"

/* Creates an empty list backed by the given value. */
no_ignore ubik_error
ubik_list_create_empty(struct ubik_value *lst)
{
        lst->tag &= TAG_TYPE_MASK;
        lst->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
        lst->left.w = 0;
        lst->right.w = 0;
        return OK;
}

/* Appends an item onto the end of the list. */
no_ignore ubik_error
ubik_list_append(struct ubik_value *lst, struct ubik_value *v)
{
        ubik_error err;

        while ((lst->tag & TAG_LEFT_WORD) == 0)
        {
                if ((lst->tag & TAG_RIGHT_NODE) == 0)
                        return ubik_raise(
                                ERR_BAD_TAG, "provided value is not a list");
                lst = lst->right.t;
        }

        lst->tag &= TAG_TYPE_MASK;
        lst->tag |= TAG_LEFT_NODE | TAG_RIGHT_NODE;
        lst->left.t = v;

        err = ubik_value_new(&lst->right.t);
        if (err != OK)
                return err;

        lst->right.t->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
        lst->right.t->left.w = 0;
        lst->right.t->right.w = 0;

        err = ubik_take(v);
        if (err != OK)
                return err;

        return OK;
}

no_ignore ubik_error
ubik_list_extend(struct ubik_value *lst, struct ubik_value *ex)
{
        size_t size;
        ubik_error err;
        ubik_word old_refcount;

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
                old_refcount = lst->refcount;
                *lst = *ex;
                lst->refcount = old_refcount;
                return OK;
        }

        while ((lst->right.t->tag & TAG_LEFT_WORD) == 0)
        {
                if ((lst->tag & TAG_RIGHT_NODE) == 0)
                        return ubik_raise(
                                ERR_BAD_TAG, "provided value is not a list");
                lst = lst->right.t;
        }

        err = ubik_release(lst->right.t);
        if (err != OK)
                return err;

        lst->right.t = ex;

        err = ubik_take(ex);
        if (err != OK)
                return err;

        return OK;
}

/* Returns the number of items in the list. */
no_ignore ubik_error
ubik_list_size(size_t *ret_size, struct ubik_value *lst)
{
        size_t size;

        for (size = 0; (lst->tag & TAG_LEFT_WORD) == 0; size++)
        {
                if ((lst->tag & TAG_RIGHT_NODE) == 0)
                        return ubik_raise(
                                ERR_BAD_TAG, "provided value is not a list");
                lst = lst->right.t;
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

        for (seen = 0; seen < i && (lst->tag & TAG_LEFT_WORD) == 0; seen++)
        {
                if ((lst->tag & TAG_RIGHT_NODE) == 0)
                        return ubik_raise(
                                ERR_BAD_TAG, "provided value is not a list");
                lst = lst->right.t;
        }

        if (lst->tag & TAG_LEFT_WORD)
                return ubik_raise(
                        ERR_OUT_OF_BOUNDS,
                        "index out of bounds in list access");
        *val = lst->left.t;
        return OK;
}
