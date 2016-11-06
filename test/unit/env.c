/*
 * env.c: run tests on environments
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
#include "unit.h"

#include <strings.h>


test_t
env()
{
        #define N_TEST_URIS 2000

        struct ubik_env env;
        struct ubik_value *v0, *v1, *r;
        struct ubik_value *t0, *t1, *rt;
        struct ubik_uri u;
        int i;
        char *key;
        struct ubik_uri uris[N_TEST_URIS];
        struct ubik_workspace *ws;
        ubik_error err;

        /* a bug in GCC < 5.0 prevents us from using = {0} to initialize the
         * uris array. */
        bzero(uris, sizeof(uris));

        assert(ubik_workspace_new(&ws) == OK);

        assert(ubik_value_new(&v0, ws) == OK);
        v0->type = UBIK_RAT;
        v0->rat.num = 0x123456789012345;
        v0->rat.den = 1;

        assert(ubik_value_new(&v1, ws) == OK);
        v1->type = UBIK_RAT;
        v1->rat.num = -10;
        v1->rat.den = 1;

        assert(ubik_value_new(&t0, ws) == OK);
        t0->type = UBIK_TYP;
        t0->typ.t = UBIK_TYPE_RAT;

        assert(ubik_value_new(&t1, ws) == OK);
        t1->type = UBIK_TYP;
        t1->typ.t = UBIK_TYPE_RAT;

        u.hash = 0;
        key = calloc(64, sizeof(char));
        snprintf(key, 64, "test_var_0");
        assert(ubik_uri_user(&u, key) == OK);
        assert(u.hash != 0);
        free(key);

        assert(ubik_env_init(&env) == OK);
        assert(ubik_env_set(&env, &u, v0, t0) == OK);

        assert(ubik_env_get(&r, &rt, &env, &u) == OK);
        assert(r == v0);

        err = ubik_env_set(&env, &u, v1, t1);
        assert(err->error_code == ERR_PRESENT);
        free(err);

        assert(ubik_env_overwrite(&env, &u, v1, t1) == OK);

        for (i = 0; i < N_TEST_URIS; i++)
        {
                key = calloc(64, sizeof(char));
                snprintf(key, 64, "test_var_%d", i);
                assert(ubik_uri_user(&uris[i], key) == OK);
                free(key);
        }

        for (i = 0; i < N_TEST_URIS; i++)
        {
                assert(ubik_env_overwrite(&env, &uris[i], v0, t0) == OK);
        }

        assert(ubik_env_free(&env) == OK);

        for (i = 0; i < N_TEST_URIS; i++)
                free(uris[i].name);
        free(u.name);

        ubik_workspace_free(ws);
        return ok;
}

run_single(env)
