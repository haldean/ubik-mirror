/*
 * natives.c: built-in native methods
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

#include "expel/assert.h"
#include "expel/dagc.h"
#include "expel/env.h"
#include "expel/expel.h"
#include "expel/natives.h"
#include "expel/types.h"
#include "expel/util.h"
#include "expel/value.h"

#include <inttypes.h>
#include <string.h>

no_ignore static xl_error
_native_uri(struct xl_uri **uri, char *name)
{
        xl_error err;
        size_t name_len;

        *uri = calloc(1, sizeof(struct xl_uri));
        if (*uri == NULL)
                return xl_raise(ERR_NO_MEMORY, "create native uri");

        name_len = strlen(name);
        if (unlikely(name_len < 1))
                return xl_raise(ERR_BAD_VALUE, "native uri must have name");

        err = xl_uri_native(*uri, name);
        return err;
}

no_ignore static xl_error
_create_op(
        struct xl_dagc **graph_ptr,
        size_t arity,
        xl_native_evaluator_t evaluator)
{
        struct xl_dagc *graph;
        struct xl_dagc_native *ngraph;
        struct xl_dagc_input *in;
        size_t i;
        xl_error err;

        err = xl_dagc_alloc(
                &graph, arity + 1, sizeof(struct xl_dagc_native), NULL);
        if (err != OK)
                return err;
        ngraph = (struct xl_dagc_native *) graph;
        *graph_ptr = graph;

        /* Create input nodes */
        for (i = 0; i < arity; i++)
        {
                in = (struct xl_dagc_input *) graph->nodes[i];
                if (in == NULL)
                        return xl_raise(ERR_NO_MEMORY, "create native dagc");
                in->head.node_type = DAGC_NODE_INPUT;
                in->head.known_type = NULL;
                in->head.known.any = NULL;
                in->head.is_terminal = 0x00;
                in->head.flags = 0x00;
                in->arg_num = i;
        }

        /* Create output native node */
        graph->nodes[arity]->node_type = DAGC_NODE_NATIVE;
        graph->nodes[arity]->known_type = NULL;
        graph->nodes[arity]->known.any = NULL;
        graph->nodes[arity]->is_terminal = 0x01;
        graph->nodes[arity]->flags = 0x00;

        graph->result = graph->nodes[arity];

        err = xl_value_new(&graph->type);
        if (err != OK)
                return err;
        graph->type->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
        graph->type->left.w = 0;
        graph->type->right.w = 0;

        err = xl_dagc_init(graph);
        if (err != OK)
                return err;
        graph->tag |= TAG_GRAPH_NATIVE;
        ngraph->evaluator = evaluator;

        return OK;
}

static xl_error
_native_unsigned_add(struct xl_env *env, struct xl_dagc *graph)
{
        struct xl_value *res;
        xl_error err;
        xl_word v0, v1;

        unused(env);

        err = xl_value_new(&res);
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
        err = xl_take(graph->result->known_type);
        if (err != OK)
                return err;

        return OK;
}

#define DEF_BINARY
#define DEF_ARG_TYPE xl_type_word
#define DEF_OP uadd
#define DEF_OP_EVAL _native_unsigned_add
#define DEF_OP_URI "uadd"
#include "expel/def-native.h"

static xl_error
_native_unsigned_subtract(struct xl_env *env, struct xl_dagc *graph)
{
        struct xl_value *res;
        xl_error err;
        xl_word v0, v1;

        unused(env);

        err = xl_value_new(&res);
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
        err = xl_take(graph->result->known_type);
        if (err != OK)
                return err;

        return OK;
}

#define DEF_BINARY
#define DEF_OP usub
#define DEF_ARG_TYPE xl_type_word
#define DEF_OP_EVAL _native_unsigned_subtract
#define DEF_OP_URI "usub"
#include "expel/def-native.h"

static xl_error
_native_concat(struct xl_env *env, struct xl_dagc *graph)
{
        xl_error err;
        struct xl_value *res;
        char *str0, *str1, *concat;
        size_t n0, n1, n;

        unused(env);

        err = xl_string_read(&str0, &n0, graph->nodes[0]->known.tree);
        if (err != OK)
                return err;
        err = xl_string_read(&str1, &n1, graph->nodes[1]->known.tree);
        if (err != OK)
                return err;

        n = n0 + n1;
        concat = calloc(n + 1, sizeof(char));
        if (concat == NULL)
                return xl_raise(ERR_NO_MEMORY, "concat alloc");
        memcpy(concat, str0, n0);
        memcpy(&concat[n0], str1, n1);

        err = xl_value_new(&res);
        if (err != OK)
                return err;
        err = xl_value_pack_string(res, concat, n);
        if (err != OK)
                return err;

        free(str0);
        free(str1);
        free(concat);

        graph->result->known.tree = res;
        graph->result->known_type = graph->nodes[0]->known_type;

        err = xl_take(graph->result->known_type);
        if (err != OK)
                return err;

        return OK;
}

#define DEF_BINARY
#define DEF_OP concat
#define DEF_ARG_TYPE xl_type_string
#define DEF_OP_EVAL _native_concat
#define DEF_OP_URI "concat"
#include "expel/def-native.h"

static xl_error
_native_emit(struct xl_env *env, struct xl_dagc *graph)
{
        xl_error err;
        char *str;
        size_t n;

        unused(env);

        err = xl_string_read(&str, &n, graph->nodes[0]->known.tree);
        if (err != OK)
                return err;
        printf("%s", str);
        free(str);

        graph->result->known.tree = graph->nodes[0]->known.tree;
        graph->result->known_type = graph->nodes[0]->known_type;

        err = xl_take(graph->result->known.tree);
        if (err != OK)
                return err;
        err = xl_take(graph->result->known_type);
        if (err != OK)
                return err;

        return OK;
}

#define DEF_UNARY
#define DEF_OP emit
#define DEF_ARG_TYPE xl_type_string
#define DEF_OP_EVAL _native_emit
#define DEF_OP_URI "emit"
#include "expel/def-native.h"

static xl_error
_native_eq(struct xl_env *env, struct xl_dagc *graph)
{
        struct xl_value *res;
        struct xl_dagc_node *n0, *n1;
        struct xl_value *v0, *v1;
        xl_tag t0, t1;
        bool ret;
        xl_error err;

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
                if (xl_value_eq(n0->known_type, n1->known_type))
                {
                        v0 = n0->known.tree;
                        v1 = n1->known.tree;
                        if (xl_type_is_prim_word(n0->known_type))
                                ret = v0->left.w == v1->left.w;
                        else
                                ret = xl_value_eq(v0, v1);
                }
        }
        else
        {
                return xl_raise(ERR_BAD_TYPE, "unknown value type in eq");
        }

        err = xl_value_new(&res);
        if (err != OK)
                return err;

        res->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
        res->left.w = ret ? 1 : 0;
        res->right.w = 0;

        graph->result->known.tree = res;

        err = xl_value_new(&graph->result->known_type);
        if (err != OK)
                return err;
        err = xl_type_bool(graph->result->known_type);
        if (err != OK)
                return err;

        /* We already own the tree and the type because we just new'ed it. */
        return OK;
}

#define DEF_BINARY
#define DEF_OP eq
#define DEF_ARG_TYPE xl_type_word
#define DEF_OP_EVAL _native_eq
#define DEF_OP_URI "eq"
#include "expel/def-native.h"

no_ignore static xl_error
_native_humanize_float(struct xl_env *env, struct xl_dagc *graph)
{
        struct xl_value *res;
        struct xl_value *type;
        xl_error err;
        char *str;
        int str_size;

        unused(env);

        err = xl_value_new(&res);
        if (err != OK)
                return err;

        err = xl_value_new(&type);
        if (err != OK)
                return err;

        str_size = asprintf(&str, "%f", res->left.f);
        if (str_size < 0)
                return xl_raise(ERR_UNEXPECTED_FAILURE, "asprintf failed");
        err = xl_value_pack_string(res, str, str_size);
        if (err != OK)
                return err;
        free(str);

        err = xl_type_string(type);
        if (err != OK)
                return err;

        graph->result->known.tree = res;
        graph->result->known_type = type;

        return OK;
}

no_ignore static xl_error
_native_humanize_word(struct xl_env *env, struct xl_dagc *graph)
{
        struct xl_value *res;
        struct xl_value *type;
        xl_error err;
        char *str;
        int str_size;

        unused(env);

        err = xl_value_new(&res);
        if (err != OK)
                return err;

        err = xl_value_new(&type);
        if (err != OK)
                return err;

        str_size = asprintf(&str, "0x%02" PRIX64, graph->nodes[0]->known.tree->left.w);
        if (str_size < 0)
                return xl_raise(ERR_UNEXPECTED_FAILURE, "asprintf failed");
        err = xl_value_pack_string(res, str, str_size);
        if (err != OK)
                return err;
        free(str);

        err = xl_type_string(type);
        if (err != OK)
                return err;

        graph->result->known.tree = res;
        graph->result->known_type = type;

        return OK;
}

no_ignore static xl_error
_register_humanize(struct xl_env *env)
{
        struct xl_dagc *wgraph;
        struct xl_dagc *fgraph;
        struct xl_value *polyfunc;

        struct xl_uri *uri;
        struct xl_value *type;
        union xl_value_or_graph ins;

        xl_error err;

        wgraph = NULL;
        err = _create_op(&wgraph, 1, _native_humanize_word);
        if (err != OK)
                return err;

        fgraph = NULL;
        err = _create_op(&fgraph, 1, _native_humanize_float);
        if (err != OK)
                return err;

        #include "humanize-poly.h"

        err = _native_uri(&uri, "humanize");
        if (err != OK)
                return err;

        wgraph->identity = uri;
        err = xl_take(uri);
        if (err != OK)
                return err;

        fgraph->identity = uri;
        err = xl_take(uri);
        if (err != OK)
                return err;

        err = xl_value_new(&type);
        if (err != OK)
                return err;
        /* TODO: set type here */

        ins.tree = polyfunc;
        err = xl_env_set(env, uri, ins, type);
        if (err != OK)
                return err;

        err = xl_release(type);
        if (err != OK)
                return err;
        err = xl_release(polyfunc);
        if (err != OK)
                return err;

        return OK;
}

no_ignore xl_error
xl_natives_register(struct xl_env *env)
{
        xl_error err;

        err = _register_uadd(env);
        if (err != OK)
                return err;

        err = _register_usub(env);
        if (err != OK)
                return err;

        err = _register_eq(env);
        if (err != OK)
                return err;

        err = _register_emit(env);
        if (err != OK)
                return err;

        err = _register_humanize(env);
        if (err != OK)
                return err;

        err = _register_concat(env);
        if (err != OK)
                return err;

        return OK;
}
