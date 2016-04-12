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


test_t
env()
{
        #define N_TEST_URIS 2000

        struct ubik_env env;
        union ubik_value_or_graph v, r;
        struct ubik_value *t, *rt;
        struct ubik_uri u;
        int i;
        char *key;
        struct ubik_uri uris[N_TEST_URIS];
        ubik_error err;

        assert(ubik_value_new(&v.tree) == OK);
        v.tree->tag = TAG_VALUE | TAG_LEFT_WORD | TAG_RIGHT_WORD;
        v.tree->left.w = 0x1234567890123456;
        v.tree->right.w = 0;

        assert(ubik_value_new(&t) == OK);
        t->tag = TAG_VALUE | TAG_LEFT_WORD | TAG_RIGHT_WORD;
        t->left.w = BASE_TYPE_WORD;
        t->right.w = 0;

        u.hash = 0;
        key = calloc(64, sizeof(char));
        snprintf(key, 64, "test_var_0");
        assert(ubik_uri_user(&u, key) == OK);
        assert(ubik_take(&u) == OK);
        assert(u.hash != 0);
        assert(u.refcount == 1);
        free(key);

        assert(ubik_env_init(&env) == OK);
        assert(ubik_env_set(&env, &u, v, t) == OK);
        assert(u.refcount == 2);

        assert(ubik_env_get(&r, &rt, &env, &u) == OK);
        assert(r.tree == v.tree);
        assert(v.tree->refcount == 2);

        err = ubik_env_set(&env, &u, v, t);
        assert(err->error_code == ERR_PRESENT);
        free(err);

        assert(v.tree->refcount == 2);

        assert(ubik_env_overwrite(&env, &u, v, t) == OK);
        assert(v.tree->refcount == 2);

        for (i = 0; i < N_TEST_URIS; i++)
        {
                key = calloc(64, sizeof(char));
                snprintf(key, 64, "test_var_%d", i);
                assert(ubik_uri_user(&uris[i], key) == OK);
                assert(ubik_take(&uris[i]) == OK);
                free(key);
        }

        for (i = 0; i < N_TEST_URIS; i++)
        {
                assert(ubik_env_overwrite(&env, &uris[i], v, t) == OK);
        }

        assert(ubik_release(t) == OK);
        assert(v.tree->refcount == N_TEST_URIS + 1);
        assert(ubik_env_free(&env) == OK);
        assert(v.tree->refcount == 1);
        assert(u.refcount == 1);
        assert(ubik_release(v.tree) == OK);

        for (i = 0; i < N_TEST_URIS; i++)
                free(uris[i].name);
        free(u.name);

        return ok;
}

run_single(env);
