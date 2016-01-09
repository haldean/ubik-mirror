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
#include "expel/util.h"

#include <wchar.h>

no_ignore static xl_error_t
__native_uri(struct xl_uri **uri, wchar_t *name)
{
        xl_error_t err;

        *uri = calloc(1, sizeof(struct xl_uri));
        if (*uri == NULL)
                return xl_raise(ERR_NO_MEMORY, "create native uri");
        err = xl_uri_native(*uri, name);
        return err;
}

no_ignore static xl_error_t
__create_op(
        struct xl_dagc **graph_ptr,
        size_t arity,
        xl_native_evaluator_t evaluator)
{
        struct xl_dagc *graph;
        struct xl_dagc_native *ngraph;
        struct xl_dagc_input *in;
        size_t i;
        xl_error_t err;

        ngraph = calloc(1, sizeof(struct xl_dagc_native));
        if (ngraph == NULL)
                return xl_raise(ERR_NO_MEMORY, "create native dagc");
        graph = (struct xl_dagc *) ngraph;
        *graph_ptr = graph;

        err = xl_dagc_alloc(graph, arity + 1);
        if (err != OK)
                return err;

        /* Create input nodes */
        for (i = 0; i < arity; i++)
        {
                in = (struct xl_dagc_input *) graph->nodes[i];
                if (in == NULL)
                        return xl_raise(ERR_NO_MEMORY, "create native dagc");
                in->head.node_type = DAGC_NODE_INPUT;
                in->head.value_type = DAGC_TYPE_UNKNOWN;
                in->head.known_type = NULL;
                in->head.known.any = NULL;
                in->head.is_terminal = 0x00;
                in->head.flags = 0x00;
                in->arg_num = i;
                err = xl_new(&in->required_type);
                if (err != OK)
                        return err;
        }

        /* Create output native node */
        graph->nodes[arity]->node_type = DAGC_NODE_NATIVE;
        graph->nodes[arity]->value_type = DAGC_TYPE_UNKNOWN;
        graph->nodes[arity]->known_type = NULL;
        graph->nodes[arity]->known.any = NULL;
        graph->nodes[arity]->is_terminal = 0x01;
        graph->nodes[arity]->flags = 0x00;

        graph->result = graph->nodes[arity];

        err = xl_dagc_init(graph);
        if (err != OK)
                return err;
        graph->tag |= TAG_NATIVE_GRAPH;
        ngraph->evaluator = evaluator;

        return OK;
}

static xl_error_t
__native_unsigned_add(struct xl_env *env, struct xl_dagc *graph)
{
        struct xl_value *res;
        xl_error_t err;

        unused(env);

        err = xl_new(&res);
        if (err != OK)
                return err;

        res->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
        res->left.v = graph->nodes[0]->known.tree->left.v +
                      graph->nodes[1]->known.tree->left.v;
        res->right.v = 0;

        graph->result->known.tree = res;
        graph->result->known_type = graph->nodes[0]->known_type;
        graph->result->value_type = DAGC_TYPE_VALUE;

        /* We already own the tree because we just new'ed it. */
        err = xl_take(graph->result->known_type);
        if (err != OK)
                return err;

        return OK;
}

no_ignore static xl_error_t
__register_unsigned_add(struct xl_env *env)
{
        xl_error_t err;

        struct xl_dagc *add_graph;
        struct xl_uri *uri;
        struct xl_value *type;
        union xl_value_or_graph ins;

        add_graph = NULL;
        err = __create_op(&add_graph, 2, __native_unsigned_add);
        if (err != OK)
                return err;
        err = __native_uri(&uri, L"uadd");
        if (err != OK)
                return err;

        err = xl_new(&type);
        if (err != OK)
                return err;

        ins.graph = add_graph;
        err = xl_set(env, uri, ins, type, DAGC_TYPE_GRAPH);
        if (err != OK)
                return err;

        err = xl_release(uri);
        if (err != OK)
                return err;
        err = xl_release(type);
        if (err != OK)
                return err;
        err = xl_release(add_graph);
        if (err != OK)
                return err;

        return OK;
}

no_ignore xl_error_t
xl_register_natives(struct xl_env *env)
{
        xl_error_t err;

        err = __register_unsigned_add(env);
        if (err != OK)
                return err;

        return OK;
}