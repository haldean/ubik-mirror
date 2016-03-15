/*
 * load_save.c: run unit tests for loading and saving values
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

#include <stdlib.h>

#include "expel/expel.h"
#include "expel/stream.h"
#include "unit.h"


test_t
load_save()
{
        struct xl_stream s;
        struct xl_value *u, *v;

        assert(xl_stream_buffer(&s) == OK);

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
        u[0].tag = TAG_VALUE | TAG_LEFT_WORD | TAG_RIGHT_NODE;
        u[0].left.w = 0x1234567890123456;
        u[0].right.t = &u[1];
        u[1].tag = TAG_VALUE | TAG_LEFT_NODE | TAG_RIGHT_WORD;
        u[1].left.t = &u[2];
        u[1].right.w = 0x456789012345678;
        u[2].tag = TAG_VALUE | TAG_LEFT_NODE | TAG_RIGHT_NODE;
        u[2].left.t = &u[3];
        u[2].right.t = &u[4];
        u[3].tag = TAG_VALUE | TAG_LEFT_NODE | TAG_RIGHT_WORD;
        u[3].left.t = &u[5];
        u[3].right.w = 0x123123123123123;
        u[4].tag = TAG_VALUE | TAG_LEFT_WORD | TAG_RIGHT_WORD;
        u[4].left.w = 0x00424242424242;
        u[4].right.w = 0x0000000000000001;
        u[5].tag = TAG_VALUE | TAG_LEFT_WORD | TAG_RIGHT_WORD;
        u[5].left.w = 0x0;
        u[5].right.w = 0xFFFFFFFFFFFFFFFF;

        assert(xl_value_save(&s, u) == OK);

        v = calloc(1, sizeof(struct xl_value));
        assert(xl_value_load(v, &s) == OK);

        /* make sure tags are correct. */
        assert(v->tag == u[0].tag);
        assert(v->right.t->tag == u[1].tag);
        assert(v->right.t->left.t->tag == u[2].tag);
        assert(v->right.t->left.t->left.t->tag == u[3].tag);
        assert(v->right.t->left.t->right.t->tag == u[4].tag);
        assert(v->right.t->left.t->left.t->left.t->tag == u[5].tag);

        /* make sure refcounts are all 1 except the root, which is not taken. */
        assert(v->refcount == 0);
        assert(v->right.t->refcount == 1);
        assert(v->right.t->left.t->refcount == 1);
        assert(v->right.t->left.t->left.t->refcount == 1);
        assert(v->right.t->left.t->right.t->refcount == 1);
        assert(v->right.t->left.t->left.t->left.t->refcount == 1);

        /* 0.left */
        assert(v->left.w == u->left.w);
        /* 5.left */
        assert(v->right.t->left.t->left.t->left.t->left.w ==
               u->right.t->left.t->left.t->left.t->left.w);
        /* 5.right */
        assert(v->right.t->left.t->left.t->left.t->right.w ==
               u->right.t->left.t->left.t->left.t->right.w);
        /* 3.right */
        assert(v->right.t->left.t->left.t->right.w ==
               u->right.t->left.t->left.t->right.w);
        /* 4.left */
        assert(v->right.t->left.t->right.t->left.w ==
               u->right.t->left.t->right.t->left.w);
        /* 4.right */
        assert(v->right.t->left.t->right.t->right.w ==
               u->right.t->left.t->right.t->right.w);
        /* 1.right */
        assert(v->right.t->right.w == u->right.t->right.w);

        xl_stream_close(&s);
        free(u);
        free(v);

        return ok;
}

run_single(load_save)
