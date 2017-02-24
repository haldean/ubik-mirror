/*
 * jobq.h: interface for MPMC job queue
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

/* The idea here is to make a series of queues, each of which has its own lock.
 * A given thread is paired with a given queue, and when a thread creates more
 * work, it pushes it onto its own queue. This decreases queue contention,
 * because usually the lock for each subqueue is only held by the thread its
 * paired with. When a thread runs out of work, it steals work from one of the
 * other threads, which involves locking the other thread's queue to steal its
 * work. */
struct ubik_jobq_node
{
        void *elem;
        struct ubik_jobq_node *next;
};

struct ubik_jobq_subq
{
        struct ubik_jobq_node *head;
        size_t size;
        pthread_mutex_t lock;
};

struct ubik_jobq
{
        struct ubik_jobq_subq *qs;
        size_t n_queues;
};

void
ubik_jobq_init(struct ubik_jobq *q, size_t n_workers);

void
ubik_jobq_push(struct ubik_jobq *q, size_t worker_id, void *e);

void *
ubik_jobq_pop(struct ubik_jobq *q, size_t worker_id);

void
ubik_jobq_free(struct ubik_jobq *q);
