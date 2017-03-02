/*
 * jobq.c: run tests on jobqs
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

#include "ubik/jobq.h"
#include "ubik/rt.h"
#include "unit.h"

test_t
jobq()
{
        struct ubik_jobq q;
        int x, y;

        ubik_jobq_init(&q, 2);
        assert(ubik_deque_empty(&q.d));

        ubik_jobq_push(&q, 0, &x);
        ubik_jobq_push(&q, 1, &y);
        assert(q.qs[0].size == 1);
        assert(q.qs[1].size == 1);
        assert(ubik_deque_empty(&q.d));

        assert(ubik_jobq_pop(&q, 0) == &x);
        assert(ubik_jobq_pop(&q, 0) == NULL);
        assert(ubik_jobq_pop(&q, 1) == &y);
        assert(ubik_jobq_pop(&q, 1) == NULL);
        assert(ubik_jobq_pop(&q, 0) == NULL);

        ubik_jobq_free(&q);
        return ok;
}

run_single(jobq)

