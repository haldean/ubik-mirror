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

#define CHECK_GLOBAL_PERIOD 20

static const size_t max_subqueue_size = 32;

void
ubik_jobq_init(struct ubik_jobq *q, size_t n_workers)
{
        ubik_deque_init(&q->d);
        ubik_galloc((void**) &q->qs, n_workers, sizeof(struct ubik_jobq_subq));
        q->n_queues = n_workers;
}

void
ubik_jobq_push(struct ubik_jobq *q, size_t worker_id, void *e)
{
        struct ubik_jobq_subq *sq;
        struct ubik_jobq_node *n;

        sq = q->qs + worker_id;

        if (unlikely(sq->size >= max_subqueue_size))
        {
                ubik_deque_pushl(&q->d, e);
                return;
        }

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

        n->right = sq->tail;
        if (sq->tail != NULL)
                sq->tail->left = n;
        else
                sq->head = n;
        sq->tail = n;
        sq->size++;
}

void *
ubik_jobq_pop(struct ubik_jobq *q, size_t worker_id)
{
        struct ubik_jobq_subq *sq;
        struct ubik_jobq_node *n;
        void *elem;

        sq = q->qs + worker_id;
        n = NULL;
        sq->since_global_check++;

        if (sq->since_global_check == CHECK_GLOBAL_PERIOD)
        {
                elem = ubik_deque_popr(&q->d);
                sq->since_global_check = 0;
                if (elem != NULL)
                        return elem;
        }

        if (sq->size > 0)
        {
                n = sq->head;
                if (n != NULL)
                {
                        sq->head = n->left;
                        if (sq->head == NULL)
                                sq->tail = NULL;
                        else
                                sq->head->right = NULL;
                        sq->size--;

                        elem = n->elem;

                        n->elem = NULL;
                        n->left = NULL;
                        n->right = sq->recycle;
                        sq->recycle = n;

                        return elem;
                }
        }

        sq->since_global_check = 0;
        return ubik_deque_popr(&q->d);
}

static void
free_ll(struct ubik_jobq_node **head)
{
        struct ubik_jobq_node *n;

        n = *head;
        while (n != NULL)
        {
                *head = n->right;
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
                free_ll(&sq->tail);
                free_ll(&sq->recycle);
        }
        free(q->qs);
}

