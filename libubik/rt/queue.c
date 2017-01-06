/*
 * queue.c: lock-free queues of exec units
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

#include <stdatomic.h>
#include <pthread.h>

#include "ubik/assert.h"
#include "ubik/queue.h"

pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;

void
ubik_queue_push(struct ubik_queue *q, struct ubik_exec_unit *u)
{
        ubik_assert(pthread_mutex_lock(&lock) == 0);
        printf("push %p:%lu\n", (void*) u->gexec, u->node);

        if (q->tail == NULL)
        {
                q->head = u;
                q->tail = u;
        }
        else
        {
                q->tail->next = u;
                q->tail = u;
        }

        ubik_assert(pthread_mutex_unlock(&lock) == 0);
}

bool
ubik_queue_pop(struct ubik_exec_unit **res, struct ubik_queue *q)
{
        bool ret;

        ubik_assert(pthread_mutex_lock(&lock) == 0);

        if (q->head == NULL)
        {
                ret = false;
        }
        else
        {
                *res = q->head;
                if (q->head == q->tail || q->head->next == NULL)
                {
                        q->head = NULL;
                        q->tail = NULL;
                }
                else
                        q->head = q->head->next;
                (*res)->next = NULL;
                ret = true;
                printf("pop  %p:%lu\n", (void*) (*res)->gexec, (*res)->node);

        }

        ubik_assert(pthread_mutex_unlock(&lock) == 0);
        return ret;
}

