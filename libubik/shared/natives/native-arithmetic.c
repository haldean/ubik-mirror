/*
 * native-arithmetic.c: built-in native arithmetic methods
 * Copyright (C) 2016, Haldean Brown
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
#include "ubik/natives.h"
#include "ubik/ubik.h"
#include "ubik/util.h"

static ubik_error
_native_unsigned_add(struct ubik_env *env, struct ubik_dagc *graph)
{
        struct ubik_value *res;
        ubik_error err;
        ubik_word v0, v1;

        unused(env);

        err = ubik_value_new(&res);
        if (err != OK)
                return err;

        v0 = graph->nodes[0]->known.tree->left.w;
        v1 = graph->nodes[1]->known.tree->left.w;

        res->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
        res->left.w = v0 + v1;
        res->right.w =
                res->left.w < v0 || res->left.w < v1
                ? ERR_OVERFLOW : 0;

        graph->result->known.tree = res;
        graph->result->known_type = graph->nodes[0]->known_type;

        /* We already own the tree because we just new'ed it. */
        err = ubik_take(graph->result->known_type);
        if (err != OK)
                return err;

        return OK;
}

#define DEF_BINARY
#define DEF_ARG_TYPE ubik_type_word
#define DEF_OP uadd
#define DEF_OP_EVAL _native_unsigned_add
#define DEF_OP_URI "uadd"
#include "ubik/def-native.h"

static ubik_error
_native_unsigned_subtract(struct ubik_env *env, struct ubik_dagc *graph)
{
        struct ubik_value *res;
        ubik_error err;
        ubik_word v0, v1;

        unused(env);

        err = ubik_value_new(&res);
        if (err != OK)
                return err;

        v0 = graph->nodes[0]->known.tree->left.w;
        v1 = graph->nodes[1]->known.tree->left.w;

        res->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
        res->left.w = v0 - v1;
        res->right.w = v0 < v1 ? ERR_UNDERFLOW : 0;

        graph->result->known.tree = res;
        graph->result->known_type = graph->nodes[0]->known_type;

        /* We already own the tree because we just new'ed it. */
        err = ubik_take(graph->result->known_type);
        if (err != OK)
                return err;

        return OK;
}

#define DEF_BINARY
#define DEF_OP usub
#define DEF_ARG_TYPE ubik_type_word
#define DEF_OP_EVAL _native_unsigned_subtract
#define DEF_OP_URI "usub"
#include "ubik/def-native.h"
