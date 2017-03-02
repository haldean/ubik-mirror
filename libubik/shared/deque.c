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

#include <stdatomic.h>

void
ubik_deque_init(struct ubik_deque *d)
{
        pthread_mutex_init(&d->lock, NULL);
        d->left = NULL;
        d->right = NULL;
}

void
ubik_deque_pushl(struct ubik_deque *d, void *e)
{
        struct ubik_deque_elem *elem;

        ubik_galloc((void**) &elem, 1, sizeof(struct ubik_deque_elem));

        pthread_mutex_lock(&d->lock);
        elem->e = e;
        elem->left = NULL;
        elem->right = d->left;
        if (elem->right != NULL)
                elem->right->left = elem;
        d->left = elem;
        if (d->right == NULL)
                d->right = elem;
        pthread_mutex_unlock(&d->lock);
}

void
ubik_deque_pushr(struct ubik_deque *d, void *e)
{
        struct ubik_deque_elem *elem;

        ubik_galloc((void**) &elem, 1, sizeof(struct ubik_deque_elem));

        pthread_mutex_lock(&d->lock);
        elem->e = e;
        elem->left = d->right;
        elem->right = NULL;
        if (elem->left != NULL)
                elem->left->right = elem;
        d->right = elem;
        if (d->left == NULL)
                d->left = elem;
        pthread_mutex_unlock(&d->lock);
}

void *
ubik_deque_popl(struct ubik_deque *d)
{
        struct ubik_deque_elem *e;
        void *v;

        pthread_mutex_lock(&d->lock);
        if (d->left == NULL)
        {
                pthread_mutex_unlock(&d->lock);
                return NULL;
        }
        e = d->left;
        d->left = e->right;
        if (e->right != NULL)
                e->right->left = NULL;
        else
                /* The case where e was the only element in the deque */
                d->right = NULL;
        pthread_mutex_unlock(&d->lock);

        v = e->e;
        free(e);
        return v;
}

void *
ubik_deque_popr(struct ubik_deque *d)
{
        struct ubik_deque_elem *e;
        void *v;

        pthread_mutex_lock(&d->lock);
        if (d->left == NULL)
        {
                pthread_mutex_unlock(&d->lock);
                return NULL;
        }
        e = d->right;
        d->right = e->left;
        if (e->left != NULL)
                e->left->right = NULL;
        else
                /* The case where e was the only element in the deque */
                d->left = NULL;
        pthread_mutex_unlock(&d->lock);

        v = e->e;
        free(e);
        return v;
}

bool
ubik_deque_empty(struct ubik_deque *d)
{
        struct ubik_deque_elem *l;

        pthread_mutex_lock(&d->lock);
        l = d->left;
        pthread_mutex_unlock(&d->lock);

        return l == NULL;
}

