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

#include "ubik/env.h"
#include "ubik/ubik.h"
#include "ubik/util.h"
#include "unit.h"

#include <inttypes.h>
#include <stdio.h>
#include <string.h>
#include <strings.h>

#define N_THREADS 8
#define THREAD_TRIALS 100
#define tassert(x) if (!(x)) { printf("thread failed: " #x "\n"); abort(); }

static void *
thread_entry(void *venv)
{
        struct ubik_env *env;
        struct ubik_value *v0, *t0;
        struct ubik_workspace *ws;
        struct ubik_uri uris[THREAD_TRIALS];
        char *key;
        size_t i;

        tassert(ubik_workspace_new(&ws) == OK);

        env = (struct ubik_env *) venv;

        for (i = 0; i < THREAD_TRIALS; i++)
        {
                tassert(ubik_value_new(&v0, ws) == OK);
                v0->type = UBIK_RAT;
                v0->rat.num = i;
                v0->rat.den = i;

                tassert(ubik_value_new(&t0, ws) == OK);
                t0->type = UBIK_TYP;
                t0->typ.t = UBIK_TYPE_RAT;

                key = calloc(64, sizeof(char));
                snprintf(key, 64, "test_var_%" PRId64 "_%" PRIuPTR,
                         ubik_gettid(), i);
                tassert(ubik_uri_user(&uris[i], key) == OK);
                free(key);

                tassert(ubik_env_set(env, &uris[i], v0, t0) == OK);
        }

        for (i = 0; i < THREAD_TRIALS; i++)
        {
                tassert(ubik_env_get(&v0, &t0, env, &uris[i]) == OK);
                tassert(v0->type == UBIK_RAT);
                tassert(v0->rat.num == (ssize_t) i);
                tassert(v0->rat.den == i);
                tassert(t0->type == UBIK_TYP);
                tassert(t0->typ.t == UBIK_TYPE_RAT);
        }

        for (i = 0; i < THREAD_TRIALS; i++)
                free(uris[i].name);

        ubik_workspace_free(ws);

        return NULL;
}

test_t
parenv()
{
        struct ubik_env env;
        pthread_t threads[N_THREADS];
        size_t thread;
        int res;

        assert(ubik_env_init(&env) == OK);

        for (thread = 0; thread < N_THREADS; thread++)
        {
                res = pthread_create(
                        &threads[thread], NULL, thread_entry, &env);
                assert(res == 0);
        }

        for (thread = 0; thread < N_THREADS; thread++)
        {
                res = pthread_join(threads[thread], NULL);
                if (res != 0)
                        printf("join failed: %s (%d)\n", strerror(res), res);
                assert(res == 0);
        }

        assert(ubik_env_free(&env) == OK);

        return ok;
}

run_single(parenv)
