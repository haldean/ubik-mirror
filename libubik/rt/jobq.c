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
#include "ubik/jobq.h"
#include "ubik/util.h"

static const size_t max_subqueue_size = 32;

static inline void
recycle(struct ubik_jobq_node *n)
{
        free(n);
}

void
ubik_jobq_init(struct ubik_jobq *q, size_t n_workers)
{
        pthread_mutex_init(&q->global_lock, NULL);
        ubik_galloc((void**) &q->qs, n_workers, sizeof(struct ubik_jobq_subq));
        q->n_queues = n_workers;
        q->global = NULL;
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
                sq->recycle = n->next;
        }
        else
                ubik_alloc1(&n, struct ubik_jobq_node, NULL);
        n->elem = e;

        if (likely(sq->size < max_subqueue_size))
        {
                n->next = sq->head;
                sq->head = n;
                sq->size++;
                return;
        }

        pthread_mutex_lock(&q->global_lock);
        n->next = q->global;
        q->global = n;
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

        if (likely(sq->size > 0))
        {
                n = sq->head;
                if (n != NULL)
                {
                        sq->head = n->next;
                        sq->size--;
                }
        }
        if (n == NULL)
        {
                pthread_mutex_lock(&q->global_lock);
                n = q->global;
                if (n != NULL)
                        q->global = n->next;
                pthread_mutex_unlock(&q->global_lock);
        }
        if (n == NULL)
                return NULL;

        elem = n->elem;

        n->elem = NULL;
        n->next = sq->recycle;
        sq->recycle = n;

        return elem;
}

static void
free_ll(struct ubik_jobq_node **head)
{
        struct ubik_jobq_node *n;

        n = *head;
        while (n != NULL)
        {
                *head = n->next;
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
        free_ll(&q->global);
        free(q->qs);
}

