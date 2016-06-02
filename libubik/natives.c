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

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include "ubik/adt.h"
#include "ubik/assert.h"
#include "ubik/dagc.h"
#include "ubik/env.h"
#include "ubik/list.h"
#include "ubik/natives.h"
#include "ubik/types.h"
#include "ubik/ubik.h"
#include "ubik/util.h"
#include "ubik/value.h"

#include <inttypes.h>
#include <string.h>

no_ignore static ubik_error
_native_uri(struct ubik_uri **uri, char *name)
{
        ubik_error err;
        size_t name_len;

        *uri = calloc(1, sizeof(struct ubik_uri));
        if (*uri == NULL)
                return ubik_raise(ERR_NO_MEMORY, "create native uri");

        name_len = strlen(name);
        if (unlikely(name_len < 1))
                return ubik_raise(ERR_BAD_VALUE, "native uri must have name");

        err = ubik_uri_native(*uri, name);
        return err;
}

no_ignore static ubik_error
_create_op(
        struct ubik_dagc **graph_ptr,
        size_t arity,
        ubik_native_evaluator_t evaluator)
{
        struct ubik_dagc *graph;
        struct ubik_dagc_native *ngraph;
        struct ubik_dagc_input *in;
        size_t i;
        ubik_error err;

        err = ubik_dagc_alloc(
                &graph, arity + 1, sizeof(struct ubik_dagc_native), NULL);
        if (err != OK)
                return err;
        ngraph = (struct ubik_dagc_native *) graph;
        *graph_ptr = graph;

        /* Create input nodes */
        for (i = 0; i < arity; i++)
        {
                in = (struct ubik_dagc_input *) graph->nodes[i];
                if (in == NULL)
                        return ubik_raise(ERR_NO_MEMORY, "create native dagc");
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

        err = ubik_dagc_init(graph);
        if (err != OK)
                return err;
        graph->tag |= TAG_GRAPH_NATIVE;
        ngraph->evaluator = evaluator;

        return OK;
}

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

static ubik_error
_native_concat(struct ubik_env *env, struct ubik_dagc *graph)
{
        ubik_error err;
        struct ubik_value *res;
        char *str0, *str1, *concat;
        size_t n0, n1, n;

        unused(env);

        err = ubik_string_read(&str0, &n0, graph->nodes[0]->known.tree);
        if (err != OK)
                return err;
        err = ubik_string_read(&str1, &n1, graph->nodes[1]->known.tree);
        if (err != OK)
                return err;

        n = n0 + n1;
        concat = calloc(n + 1, sizeof(char));
        if (concat == NULL)
                return ubik_raise(ERR_NO_MEMORY, "concat alloc");
        memcpy(concat, str0, n0);
        memcpy(&concat[n0], str1, n1);

        err = ubik_value_new(&res);
        if (err != OK)
                return err;
        err = ubik_value_pack_string(res, concat, n);
        if (err != OK)
                return err;

        free(str0);
        free(str1);
        free(concat);

        graph->result->known.tree = res;
        graph->result->known_type = graph->nodes[0]->known_type;

        err = ubik_take(graph->result->known_type);
        if (err != OK)
                return err;

        return OK;
}

#define DEF_BINARY
#define DEF_OP concat
#define DEF_ARG_TYPE ubik_type_string
#define DEF_OP_EVAL _native_concat
#define DEF_OP_URI "concat"
#include "ubik/def-native.h"

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

no_ignore static ubik_error
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

static ubik_error
_native_adt_new(struct ubik_env *env, struct ubik_dagc *graph)
{
        struct ubik_value *type_decl;
        struct ubik_value *ctor;
        struct ubik_value *args;
        struct ubik_value *res;
        ubik_error err, cerr;
        size_t i;
        unused(env);

        type_decl = graph->nodes[0]->known.tree;
        ctor = graph->nodes[1]->known.tree;

        err = ubik_value_new(&args);
        if (err != OK)
                return err;
        err = ubik_list_create_empty(args);
        if (err != OK)
                goto free_args;

        for (i = 2; i < graph->n - 1; i++)
        {
                err = ubik_list_append(args, graph->nodes[i]->known.tree);
                if (err != OK)
                        goto free_args;
        }

        err = ubik_value_new(&res);
        if (err != OK)
                return err;
        err = ubik_adt_instantiate(res, type_decl, ctor, args);
        if (err != OK)
                goto free_args;
        graph->result->known.tree = res;

        graph->result->known_type = type_decl;
        err = ubik_take(type_decl);
        if (err != OK)
                goto free_args;

free_args:
        cerr = ubik_release(args);
        if (cerr != OK)
                return err ? err : cerr;

        return err;
}

static ubik_error
_register_all_adt_new(struct ubik_env *env)
{
        struct ubik_dagc *graph;
        struct ubik_uri *uri;
        struct ubik_value *type;
        union ubik_value_or_graph ins;
        ubik_error err;
        char *func_name;
        int res;
        int i;

        for (i = 0; i < UBIK_MAX_ADT_FIELDS; i++)
        {
                graph = NULL;
                err = _create_op(&graph, i + 2, _native_adt_new);
                if (err != OK)
                        return err;

                res = asprintf(&func_name, "ubik-adt-new-%d", i);
                if (res < 0)
                        return ubik_raise(ERR_NO_MEMORY, "adt new name alloc");
                err = _native_uri(&uri, func_name);
                if (err != OK)
                        return err;
                free(func_name);

                graph->identity = uri;
                err = ubik_take(graph->identity);
                if (err != OK)
                        return err;

                err = ubik_value_new(&type);
                if (err != OK)
                        return err;

                ins.graph = graph;
                err = ubik_env_set(env, uri, ins, type);
                if (err != OK)
                        return err;

                err = ubik_release(type);
                if (err != OK)
                        return err;
                err = ubik_release(graph);
                if (err != OK)
                        return err;
        }

        return OK;
}

no_ignore static ubik_error
_native_adt_ctor_matches(struct ubik_env *env, struct ubik_dagc *graph)
{
        struct ubik_value *inst;
        struct ubik_value *match_name_val;
        char *match_name;
        char *ctor_name;
        size_t match_name_len;
        bool matches;
        ubik_error err;
        unused(env);

        match_name_val = graph->nodes[0]->known.tree;
        inst = graph->nodes[1]->known.tree;
        err = OK;

        err = ubik_string_read(&match_name, &match_name_len, match_name_val);
        if (err != OK)
                return err;

        err = ubik_adt_get_ctor(&ctor_name, inst);
        if (err != OK)
                goto free_match_name;

        matches = strncmp(match_name, ctor_name, match_name_len) == 0;

        err = ubik_value_new(&graph->result->known.tree);
        if (err != OK)
                goto free_ctor_name;
        graph->result->known.tree->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
        graph->result->known.tree->left.w = matches ? 1 : 0;

        err = ubik_value_new(&graph->result->known_type);
        if (err != OK)
                goto free_ctor_name;
        err = ubik_type_bool(graph->result->known_type);
        if (err != OK)
                goto free_ctor_name;

free_ctor_name:
        free(ctor_name);
free_match_name:
        free(match_name);

        return err;
}

static ubik_error
_register_adt_ctor_matches(struct ubik_env *env)
{
        struct ubik_dagc *graph;
        struct ubik_uri *uri;
        struct ubik_value *type;
        union ubik_value_or_graph ins;
        ubik_error err;

        graph = NULL;
        err = _create_op(&graph, 2, _native_adt_ctor_matches);
        if (err != OK)
                return err;

        err = _native_uri(&uri, "ubik-adt-ctor-matches?");
        if (err != OK)
                return err;

        graph->identity = uri;
        err = ubik_take(graph->identity);
        if (err != OK)
                return err;

        err = ubik_value_new(&type);
        if (err != OK)
                return err;

        ins.graph = graph;
        err = ubik_env_set(env, uri, ins, type);
        if (err != OK)
                return err;

        err = ubik_release(type);
        if (err != OK)
                return err;
        err = ubik_release(graph);
        if (err != OK)
                return err;
        return OK;
}

static char *native_names[] = {
        "uadd",
        "usub",
        "eq",
        "emit",
        "humanize",
        "concat",
        "ubik-adt-ctor-matches?",
};

no_ignore ubik_error
ubik_natives_register(struct ubik_env *env)
{
        ubik_error err;

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

        err = _register_all_adt_new(env);
        if (err != OK)
                return err;

        err = _register_adt_ctor_matches(env);
        if (err != OK)
                return err;

        return OK;
}

bool
ubik_natives_is_defined(char *name)
{
        size_t i;
        size_t n;
        n = sizeof(native_names) / sizeof(native_names[0]);
        for (i = 0; i < n; i++)
                if (strcmp(native_names[i], name) == 0)
                        return true;
        return false;
}
