/*
 * deque.h: double-sided queue implementation
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

#pragma once
#include "ubik/alloc.h"
#include <pthread.h>

struct ubik_deque_elem
{
        void *e;
        struct ubik_deque_elem *left;
        struct ubik_deque_elem *right;
};

struct ubik_deque
{
        struct ubik_deque_elem *left;
        struct ubik_deque_elem *right;
        struct ubik_alloc_region *r;
        pthread_mutex_t lock;
};

void
ubik_deque_init(struct ubik_deque *d);

void
ubik_deque_pushl(struct ubik_deque *d, void *e);

void
ubik_deque_pushr(struct ubik_deque *d, void *e);

void *
ubik_deque_popl(struct ubik_deque *d);

void *
ubik_deque_popr(struct ubik_deque *d);

bool
ubik_deque_empty(struct ubik_deque *d);
