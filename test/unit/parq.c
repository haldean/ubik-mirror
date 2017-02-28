/*
 * parenv.c: run tests on environments in parallel, to check locking
 * Copyright (C) 2015, Haldean Brown
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
#include "ubik/ubik.h"
#include "ubik/util.h"
#include "unit.h"

#include <inttypes.h>
#include <stdio.h>
#include <string.h>
#include <strings.h>

#define N_THREADS 16
#define THREAD_INSERTS 5000
#define tassert(x) if (!(x)) { printf("thread failed: " #x "\n"); abort(); }

static _Atomic uintptr_t elem = 1;
static _Atomic bool seen[N_THREADS * THREAD_INSERTS + 1] = {0};

struct thread_info
{
        struct ubik_jobq *q;
        size_t wid;
};

static void *
thread_entry(void *vti)
{
        struct thread_info *ti;
        struct ubik_jobq *q;
        size_t i;
        size_t wid;
        uintptr_t r;
        uintptr_t count;

        ti = (struct thread_info *) vti;
        q = ti->q;
        wid = ti->wid;
        free(ti);

        for (i = 0; i < THREAD_INSERTS; i++)
        {
                r = elem++;
                ubik_jobq_push(q, wid, (void *) r);
        }

        count = 0;
        for (;;)
        {
                r = (uintptr_t) ubik_jobq_pop(q, wid);
                if (r == 0)
                        break;
                seen[r] = true;
                count++;
        }

        return (void *) i;
}

test_t
parq()
{
        struct ubik_jobq q;
        pthread_t threads[N_THREADS];
        size_t thread;
        size_t total;
        size_t i;
        int res;
        void *ret;
        struct thread_info *ti;

        ubik_jobq_init(&q, N_THREADS);

        for (thread = 0; thread < N_THREADS; thread++)
        {
                ti = calloc(1, sizeof(struct thread_info));
                ti->q = &q;
                ti->wid = thread;
                res = pthread_create(&threads[thread], NULL, thread_entry, ti);
                assert(res == 0);
        }

        total = 0;
        for (thread = 0; thread < N_THREADS; thread++)
        {
                res = pthread_join(threads[thread], &ret);
                if (res != 0)
                        printf("join failed: %s (%d)\n", strerror(res), res);
                assert(res == 0);
                total += (uintptr_t) ret;
        }

        ubik_jobq_free(&q);
        printf("total: %lu\n", total);
        assert(total == N_THREADS * THREAD_INSERTS);

        for (i = 1; i < N_THREADS * THREAD_INSERTS + 1; i++)
                assert(seen[i]);

        return ok;
}

run_single(parq)
