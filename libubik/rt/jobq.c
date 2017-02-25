/*
 * jobq.c: implementation for MPMC job queue
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

#include "ubik/alloc.h"
#include "ubik/assert.h"
#include "ubik/jobq.h"
#include "ubik/util.h"

static const size_t max_subqueue_size = 32;

void
ubik_jobq_init(struct ubik_jobq *q, size_t n_workers)
{
        size_t i;

        pthread_mutex_init(&q->global_lock, NULL);
        ubik_galloc((void**) &q->qs, n_workers, sizeof(struct ubik_jobq_subq));
        q->n_queues = n_workers;
        q->global_head = NULL;
        q->global_tail = NULL;

        for (i = 0; i < n_workers; i++)
                q->qs[i].owner_tid = -1;
}

void
ubik_jobq_push(struct ubik_jobq *q, size_t worker_id, void *e)
{
        struct ubik_jobq_subq *sq;
        struct ubik_jobq_node *n;

        sq = q->qs + worker_id;
        if (sq->recycle != NULL)
        {
                n = sq->recycle;
                sq->recycle = n->right;
        }
        else
        {
                n = calloc(1, sizeof(struct ubik_jobq_node));
                ubik_assert(n != NULL);
        }
        n->elem = e;

        if (likely(sq->size < max_subqueue_size))
        {
                n->right = sq->tail;
                if (sq->tail != NULL)
                        sq->tail->left = n;
                if (sq->head == NULL)
                        sq->head = n;
                sq->tail = n;
                sq->size++;
                return;
        }

        pthread_mutex_lock(&q->global_lock);
        n->right = q->global_tail;
        if (q->global_tail != NULL)
                q->global_tail->left = n;
        if (q->global_head == NULL)
                q->global_head = n;
        q->global_tail = n;
        pthread_mutex_unlock(&q->global_lock);
}

void *
ubik_jobq_pop(struct ubik_jobq *q, size_t worker_id)
{
        struct ubik_jobq_subq *sq;
        struct ubik_jobq_node *n;
        void *elem;

        sq = q->qs + worker_id;
        n = NULL;

        if (sq->owner_tid == -1)
                sq->owner_tid = ubik_gettid();
        else if (ubik_gettid() != sq->owner_tid)
        {
                ubik_unreachable("shouldn't happen!");
        }

        if (likely(sq->size > 0))
        {
                n = sq->head;
                if (n != NULL)
                {
                        sq->head = n->left;
                        sq->size--;
                        if (!sq->size)
                                sq->tail = NULL;
                }
        }
        if (n == NULL)
        {
                pthread_mutex_lock(&q->global_lock);
                n = q->global_head;
                if (n != NULL)
                        q->global_head = n->left;
                if (q->global_head == NULL)
                        q->global_tail = NULL;
                pthread_mutex_unlock(&q->global_lock);
        }
        if (n == NULL)
                return NULL;

        elem = n->elem;

        /*
        n->elem = NULL;
        n->right = sq->recycle;
        sq->recycle = n;
        */
        free(n);

        return elem;
}

static void
free_ll(struct ubik_jobq_node **head)
{
        struct ubik_jobq_node *n;

        n = *head;
        while (n != NULL)
        {
                *head = n->left;
                free(n);
                n = *head;
        }
}

void
ubik_jobq_free(struct ubik_jobq *q)
{
        size_t i;
        struct ubik_jobq_subq *sq;

        for (i = 0; i < q->n_queues; i++)
        {
                sq = q->qs + i;
                free_ll(&sq->head);
                free_ll(&sq->recycle);
        }
        free_ll(&q->global_head);
        free(q->qs);
}

