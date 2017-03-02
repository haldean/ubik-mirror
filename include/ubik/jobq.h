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

/*
   The idea here is to make a series of fixed-sized queues, each of which is
   owned by a given thread. A thread pulls jobs from its own queue, and when a
   thread creates more work, it pushes it onto its own queue. This decreases
   queue contention, because usually the lock for each subqueue is only held by
   the thread its paired with. When a thread runs out of work, it fills its
   queue from the global queue, which sits behind a lock. When a thread tries
   to push onto its queue but its queue is full, it pushes directly onto the
   global queue.
 */

#include "ubik/deque.h"

#include <pthread.h>
#include <stdlib.h>

struct ubik_jobq_node
{
        void *elem;
        /* left points towards tail, right point towards head. */
        struct ubik_jobq_node *left;
        struct ubik_jobq_node *right;
};

struct ubik_jobq_subq
{
        /* elements are pushed onto the tail and popped off the head */
        struct ubik_jobq_node *head;
        struct ubik_jobq_node *tail;
        struct ubik_jobq_node *recycle;
        size_t size;

        /* This counts the number of pops we've taken from the local queue
         * without checking the global queue. When this hits a threshold value,
         * we check the global queue for work. This prevents the evaluator from
         * spinning on the local queue while the tasks that are blocking those
         * local tasks from being evaluated are sitting on the global queue not
         * getting done. */
        uint16_t since_global_check;
};

struct ubik_jobq
{
        struct ubik_deque d;
        struct ubik_jobq_subq *qs;
        size_t n_queues;
};

void
ubik_jobq_init(struct ubik_jobq *q, size_t n_workers);

void
ubik_jobq_push(struct ubik_jobq *q, size_t worker_id, void *e);

void *
ubik_jobq_pop(struct ubik_jobq *q, size_t worker_id);

/* This is not MT-Safe; no other jobq operations can be interleaved with a
 * free, or your program will almost certainly segfault. */
void
ubik_jobq_free(struct ubik_jobq *q);
