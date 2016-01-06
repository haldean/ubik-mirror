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
#include "expel/expel.h"
#include "expel/gc.h"
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

        assert(xl_save_value(&s, u) == OK);

        v = calloc(1, sizeof(struct xl_value));
        assert(xl_load_value(v, &s) == OK);

        // make sure tags are correct.
        assert(v->tag == u[0].tag);
        assert(v->right.p->tag == u[1].tag);
        assert(v->right.p->left.p->tag == u[2].tag);
        assert(v->right.p->left.p->left.p->tag == u[3].tag);
        assert(v->right.p->left.p->right.p->tag == u[4].tag);
        assert(v->right.p->left.p->left.p->left.p->tag == u[5].tag);

        // make sure refcounts are all 1 except the root, which is not taken.
        assert(v->refcount == 0);
        assert(v->right.p->refcount == 1);
        assert(v->right.p->left.p->refcount == 1);
        assert(v->right.p->left.p->left.p->refcount == 1);
        assert(v->right.p->left.p->right.p->refcount == 1);
        assert(v->right.p->left.p->left.p->left.p->refcount == 1);

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
        struct xl_value *v, *r, *t, *rt;
        struct xl_uri u;
        int i;
        wchar_t *key;
        struct xl_uri uris[N_TEST_URIS];

        assert(xl_new(&v) == OK);
        v->tag = TAG_LEFT_WORD | TAG_RIGHT_WORD;
        v->left.v = 0x1234567890123456;
        v->right.v = 0;

        assert(xl_new(&t) == OK);
        t->tag = TAG_LEFT_WORD | TAG_RIGHT_WORD;
        t->left.v = BASE_TYPE_WORD;
        t->right.v = 0;

        u.hash = 0;
        key = calloc(64, sizeof(wchar_t));
        swprintf(key, 64, L"test_var_0");
        assert(xl_uri_local(&u, key) == OK);
        assert(u.hash != 0);

        assert(xl_env_init(&env) == OK);
        assert(xl_set(&env, &u, v, t) == OK);
        assert(xl_get(&r, &rt, &env, &u) == OK);
        assert(r == v);
        assert(v->refcount == 2);

        assert(xl_set(&env, &u, v, t) == ERR_PRESENT);
        assert(v->refcount == 2);

        assert(xl_overwrite(&env, &u, v, t) == OK);
        assert(v->refcount == 2);

        for (i = 0; i < N_TEST_URIS; i++)
        {
                key = calloc(64, sizeof(wchar_t));
                swprintf(key, 64, L"test_var_%d", i);
                assert(xl_uri_local(&uris[i], key) == OK);
        }

        for (i = 0; i < N_TEST_URIS; i++)
        {
                assert(xl_overwrite(&env, &uris[i], v, t) == OK);
        }

        assert(v->refcount == N_TEST_URIS + 1);
        assert(xl_env_free(&env) == OK);
        assert(v->refcount == 1);
        return ok;
}

test_t
gc()
{
        #define N_TEST_GC_VALUES 10000

        size_t i;
        struct xl_value *vals[N_TEST_GC_VALUES];
        struct xl_gc_info gc_stats;

        xl_gc_free_all();
        xl_gc_start();

        for (i = 0; i < N_TEST_GC_VALUES; i++)
        {
                assert(xl_new(&vals[i]) == 0);
        }
        for (i = 0; i < N_TEST_GC_VALUES; i++)
        {
                assert(xl_release(vals[i]) == 0);
        }

        xl_gc_get_stats(&gc_stats);
        #ifdef XL_GC_DEBUG
                assert(gc_stats.n_val_allocs == N_TEST_GC_VALUES);
                assert(gc_stats.n_val_frees == N_TEST_GC_VALUES);
                assert(gc_stats.n_val_frees >= gc_stats.n_gc_runs);
        #endif

        xl_gc_free_all();
        xl_gc_start();

        for (i = 0; i < XL_GC_PAGE_SIZE * 2; i++)
        {
                assert(xl_new(&vals[i]) == 0);
        }
        for (i = 0; i < XL_GC_PAGE_SIZE * 2; i++)
        {
                assert(xl_release(vals[i]) == 0);
        }
        for (i = 0; i < XL_GC_PAGE_SIZE * 2; i++)
        {
                assert(xl_new(&vals[i]) == 0);
        }

        xl_gc_get_stats(&gc_stats);
        #ifdef XL_GC_DEBUG
                assert(gc_stats.n_page_allocs == 2);
        #endif

        return ok;
}

int
main()
{
        word_t err;
        init();
        if ((err = xl_start()) != OK)
        {
                printf("couldn't start expel: %s\n", explain_word(err));
                return -1;
        }
        run(buffer);
        run(load_save);
        run(host_to_net);
        run(env);
        run(gc);
        finish();
}
