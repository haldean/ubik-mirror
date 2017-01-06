/*
 * queue.c: run tests on queues
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

#include "ubik/ubik.h"
#include "ubik/queue.h"
#include "unit.h"

test_t
queue()
{
        struct ubik_exec_unit u0 = {0}, u1 = {0}, u2 = {0}, u3 = {0};
        struct ubik_exec_unit *r;
        struct ubik_queue q = {0};

        ubik_queue_push(&q, &u0);
        ubik_queue_push(&q, &u1);
        ubik_queue_push(&q, &u2);
        ubik_queue_push(&q, &u3);

        assert(ubik_queue_pop(&r, &q));
        assert(r == &u0);
        assert(ubik_queue_pop(&r, &q));
        assert(r == &u1);
        assert(ubik_queue_pop(&r, &q));
        assert(r == &u2);
        assert(ubik_queue_pop(&r, &q));
        assert(r == &u3);
        assert(!ubik_queue_pop(&r, &q));

        return ok;
}

run_single(queue)
