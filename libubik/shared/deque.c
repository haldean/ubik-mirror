/*
 * deque.c: double-sided queue implementation
 * Copyright (C) 2017, Haldean Brown
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

#include "ubik/assert.h"
#include "ubik/deque.h"

void
ubik_deque_pushl(struct ubik_deque *d, void *e)
{
        struct ubik_deque_elem *elem;
        ubik_alloc1(&elem, struct ubik_deque_elem, d->r);
        elem->e = e;
        elem->left = NULL;
        elem->right = d->left;
        d->left = elem;
        if (d->right == NULL)
                d->right = elem;
}

void
ubik_deque_pushr(struct ubik_deque *d, void *e)
{
        struct ubik_deque_elem *elem;
        ubik_alloc1(&elem, struct ubik_deque_elem, d->r);
        elem->e = e;
        elem->left = d->right;
        elem->right = NULL;
        d->right = elem;
        if (d->left == NULL)
                d->left = elem;
}

void *
ubik_deque_popl(struct ubik_deque *d)
{
        struct ubik_deque_elem *e;
        void *v;

        if (ubik_deque_empty(d))
                return NULL;
        ubik_assert(d->left != NULL);

        e = d->left;
        d->left = e->right;
        if (e->right != NULL)
                e->right->left = NULL;
        else
                /* The case where e was the only element in the deque */
                d->right = NULL;

        v = e->e;
        ubik_free(d->r, e);
        return v;
}

void *
ubik_deque_popr(struct ubik_deque *d)
{
        struct ubik_deque_elem *e;
        void *v;

        if (ubik_deque_empty(d))
                return NULL;
        ubik_assert(d->right != NULL);

        e = d->right;
        d->right = e->left;
        if (e->left != NULL)
                e->left->right = NULL;
        else
                /* The case where e was the only element in the deque */
                d->left = NULL;

        v = e->e;
        ubik_free(d->r, e);
        return v;
}

bool
ubik_deque_empty(struct ubik_deque *d)
{
        return d->left == NULL;
}

