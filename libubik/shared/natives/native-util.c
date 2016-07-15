/*
 * native-util.c: built-in native methods
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
#include "ubik/natives.h"
#include "ubik/types.h"
#include "ubik/ubik.h"
#include "ubik/util.h"
#include "ubik/value.h"

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <string.h>

static ubik_error
_native_emit(struct ubik_env *env, struct ubik_dagc *graph)
{
        ubik_error err;
        char *str;
        size_t n;

        unused(env);

        err = ubik_string_read(&str, &n, graph->nodes[0]->known.tree);
        if (err != OK)
                return err;
        printf("%s", str);
        free(str);

        graph->result->known.tree = graph->nodes[0]->known.tree;
        graph->result->known_type = graph->nodes[0]->known_type;

        err = ubik_take(graph->result->known.tree);
        if (err != OK)
                return err;
        err = ubik_take(graph->result->known_type);
        if (err != OK)
                return err;

        return OK;
}

#define DEF_UNARY
#define DEF_OP emit
#define DEF_ARG_TYPE ubik_type_string
#define DEF_OP_EVAL _native_emit
#define DEF_OP_URI "emit"
#include "ubik/def-native.h"

static ubik_error
_native_eq(struct ubik_env *env, struct ubik_dagc *graph)
{
        struct ubik_value *res;
        struct ubik_dagc_node *n0, *n1;
        struct ubik_value *v0, *v1;
        ubik_tag t0, t1;
        bool ret;
        ubik_error err;

        unused(env);

        n0 = graph->nodes[0];
        n1 = graph->nodes[1];
        t0 = *n0->known.tag;
        t1 = *n1->known.tag;

        ret = true;

        if ((t0 & TAG_TYPE_MASK) != (t1 & TAG_TYPE_MASK))
        {
                ret = false;
        }
        else if ((t0 & TAG_TYPE_MASK) == TAG_GRAPH)
        {
                /* TODO: make this a semantic comparison */
                ret = n0->known.graph == n1->known.graph;
        }
        else if ((t0 & TAG_TYPE_MASK) == TAG_VALUE)
        {
                ret = false;
                if (ubik_value_eq(n0->known_type, n1->known_type))
                {
                        v0 = n0->known.tree;
                        v1 = n1->known.tree;
                        if (ubik_type_is_prim_word(n0->known_type))
                                ret = v0->left.w == v1->left.w;
                        else
                                ret = ubik_value_eq(v0, v1);
                }
        }
        else
        {
                return ubik_raise(ERR_BAD_TYPE, "unknown value type in eq");
        }

        err = ubik_value_new(&res);
        if (err != OK)
                return err;

        res->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
        res->left.w = ret ? 1 : 0;
        res->right.w = 0;

        graph->result->known.tree = res;

        err = ubik_value_new(&graph->result->known_type);
        if (err != OK)
                return err;
        err = ubik_type_bool(graph->result->known_type);
        if (err != OK)
                return err;

        /* We already own the tree and the type because we just new'ed it. */
        return OK;
}

#define DEF_BINARY
#define DEF_OP eq
#define DEF_ARG_TYPE ubik_type_word
#define DEF_OP_EVAL _native_eq
#define DEF_OP_URI "eq"
#include "ubik/def-native.h"

no_ignore static ubik_error
_native_humanize_float(struct ubik_env *env, struct ubik_dagc *graph)
{
        struct ubik_value *res;
        struct ubik_value *type;
        ubik_error err;
        char *str;
        int str_size;

        unused(env);

        err = ubik_value_new(&res);
        if (err != OK)
                return err;

        err = ubik_value_new(&type);
        if (err != OK)
                return err;

        str_size = asprintf(&str, "%f", res->left.f);
        if (str_size < 0)
                return ubik_raise(ERR_UNEXPECTED_FAILURE, "asprintf failed");
        err = ubik_value_pack_string(res, str, str_size);
        if (err != OK)
                return err;
        free(str);

        err = ubik_type_string(type);
        if (err != OK)
                return err;

        graph->result->known.tree = res;
        graph->result->known_type = type;

        return OK;
}

no_ignore static ubik_error
_native_humanize_word(struct ubik_env *env, struct ubik_dagc *graph)
{
        struct ubik_value *res;
        struct ubik_value *type;
        ubik_error err;
        char *str;
        int str_size;

        unused(env);

        err = ubik_value_new(&res);
        if (err != OK)
                return err;

        err = ubik_value_new(&type);
        if (err != OK)
                return err;

        str_size = asprintf(&str, "0x%02" PRIX64, graph->nodes[0]->known.tree->left.w);
        if (str_size < 0)
                return ubik_raise(ERR_UNEXPECTED_FAILURE, "asprintf failed");
        err = ubik_value_pack_string(res, str, str_size);
        if (err != OK)
                return err;
        free(str);

        err = ubik_type_string(type);
        if (err != OK)
                return err;

        graph->result->known.tree = res;
        graph->result->known_type = type;

        return OK;
}

no_ignore ubik_error
_register_humanize(struct ubik_env *env)
{
        struct ubik_dagc *wgraph;
        struct ubik_dagc *fgraph;
        struct ubik_value *polyfunc;

        struct ubik_uri *uri;
        struct ubik_value *type;
        union ubik_value_or_graph ins;

        ubik_error err;

        wgraph = NULL;
        err = ubik_internal_native_create_op(
                &wgraph, 1, _native_humanize_word);
        if (err != OK)
                return err;

        fgraph = NULL;
        err = ubik_internal_native_create_op(
                &fgraph, 1, _native_humanize_float);
        if (err != OK)
                return err;

        #include "humanize-poly.h"

        err = ubik_internal_native_uri(&uri, "humanize");
        if (err != OK)
                return err;

        wgraph->identity = uri;
        err = ubik_take(uri);
        if (err != OK)
                return err;

        fgraph->identity = uri;
        err = ubik_take(uri);
        if (err != OK)
                return err;

        err = ubik_value_new(&type);
        if (err != OK)
                return err;
        /* TODO: set type here */

        ins.tree = polyfunc;
        err = ubik_env_set(env, uri, ins, type);
        if (err != OK)
                return err;

        err = ubik_release(type);
        if (err != OK)
                return err;
        err = ubik_release(polyfunc);
        if (err != OK)
                return err;

        return OK;
}
