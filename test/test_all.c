/*
 * test_all.c: run all expelc tests
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

#include <stdio.h>
#include <stdlib.h>

#include "expel/env.h"
#include "expel/stream.h"
#include "expel/util.h"
#include "unit.h"

test_t
buffer()
{
        struct xl_stream s;
        char c[20];
        size_t n;

        xl_stream_buffer(&s);

        n = xl_stream_write(&s, (char[]){0, 1, 2, 3, 4}, 5);
        assert(n == 5);

        n = xl_stream_read(c, &s, 5);
        assert(n == 5);

        assert(c[0] == 0);
        assert(c[1] == 1);
        assert(c[2] == 2);
        assert(c[3] == 3);
        assert(c[4] == 4);

        n = xl_stream_read(c, &s, 5);
        assert(n == 0);

        return ok;
}

test_t
load_save()
{
        struct xl_stream s;
        struct xl_value *u, *v;

        xl_stream_buffer(&s);

        /*
         *              0
         *             / \
         *            W   1
         *               / \
         *              2   W
         *             / \
         *            /   \
         *           3     4
         *          / \   / \
         *         5   W W   W
         *        / \
         *       W   W
         */
        u = calloc(6, sizeof(struct xl_value));
        u[0].tag = TAG_LEFT_WORD | TAG_RIGHT_NODE;
        u[0].left.v = 0x1234567890123456;
        u[0].right.p = &u[1];
        u[1].tag = TAG_LEFT_NODE | TAG_RIGHT_WORD;
        u[1].left.p = &u[2];
        u[1].right.v = 0x456789012345678;
        u[2].tag = TAG_LEFT_NODE | TAG_RIGHT_NODE;
        u[2].left.p = &u[3];
        u[2].right.p = &u[4];
        u[3].tag = TAG_LEFT_NODE | TAG_RIGHT_WORD;
        u[3].left.p = &u[5];
        u[3].right.v = 0x123123123123123;
        u[4].tag = TAG_LEFT_WORD | TAG_RIGHT_WORD;
        u[4].left.v = 0x00424242424242;
        u[4].right.v = 0x0000000000000001;
        u[5].tag = TAG_LEFT_WORD | TAG_RIGHT_WORD;
        u[5].left.v = 0x0;
        u[5].right.v = 0xFFFFFFFFFFFFFFFF;

        assert(xl_save(&s, u) == OK);

        v = calloc(1, sizeof(struct xl_value));
        assert(xl_load(v, &s) == OK);

        assert(v->tag == u[0].tag);
        assert(v->right.p->tag == u[1].tag);
        assert(v->right.p->left.p->tag == u[2].tag);
        assert(v->right.p->left.p->left.p->tag == u[3].tag);
        assert(v->right.p->left.p->right.p->tag == u[4].tag);
        assert(v->right.p->left.p->left.p->left.p->tag == u[5].tag);

        // 0.left
        assert(v->left.v == u->left.v);
        // 5.left
        assert(v->right.p->left.p->left.p->left.p->left.v ==
               u->right.p->left.p->left.p->left.p->left.v);
        // 5.right
        assert(v->right.p->left.p->left.p->left.p->right.v ==
               u->right.p->left.p->left.p->left.p->right.v);
        // 3.right
        assert(v->right.p->left.p->left.p->right.v ==
               u->right.p->left.p->left.p->right.v);
        // 4.left
        assert(v->right.p->left.p->right.p->left.v ==
               u->right.p->left.p->right.p->left.v);
        // 4.right
        assert(v->right.p->left.p->right.p->right.v ==
               u->right.p->left.p->right.p->right.v);
        // 1.right
        assert(v->right.p->right.v == u->right.p->right.v);

        return ok;
}

test_t
host_to_net()
{
        word_t v;

        v = 0x0123456789ABCDEF;
        v = htonw(v);
        assert(*((uint8_t *) &v) == 0x01);
        assert(*((uint8_t *) &v + 7) == 0xEF);

        v = ntohw(v);
        assert(v == 0x0123456789ABCDEF);

        return ok;
}

test_t
env()
{
        #define N_TEST_URIS 2000

        struct xl_env env;
        struct xl_value v, *r;
        struct xl_uri u;
        int i;
        char *key;
        struct xl_uri uris[N_TEST_URIS];

        v.tag = TAG_LEFT_WORD | TAG_RIGHT_WORD;
        v.left.v = 0x1234567890123456;
        v.right.v = 0xFFFFFFFFFFFFFFFF;

        u.hash = 0;
        xl_uri_local(&u, "test_var_00000");
        assert(u.hash != 0);

        xl_env_init(&env);
        assert(xl_set(&env, &u, &v) == OK);
        assert(xl_get(&r, &env, &u) == OK);
        assert(r == &v);

        for (i = 0; i < N_TEST_URIS; i++)
        {
                key = malloc(64);
                sprintf(key, "test_var_%d", i);
                xl_uri_local(&uris[i], key);
        }
        for (i = 0; i < N_TEST_URIS; i++)
        {
                assert(xl_set(&env, &uris[i], &v) == OK);
        }

        xl_env_free(&env);
        return ok;
}

int
main()
{
        init();
        run(buffer);
        run(load_save);
        run(host_to_net);
        run(env);
        finish();
}
