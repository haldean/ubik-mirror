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

#include <string.h>
#include <wchar.h>

no_ignore static xl_error_t
_native_uri(struct xl_uri **uri, wchar_t *name)
{
        xl_error_t err;
        wchar_t *heap_name;
        size_t name_len;

        *uri = calloc(1, sizeof(struct xl_uri));
        if (*uri == NULL)
                return xl_raise(ERR_NO_MEMORY, "create native uri");

        name_len = wcslen(name);
        if (unlikely(name_len < 1))
                return xl_raise(ERR_BAD_VALUE, "native uri must have name");

        /* URI memory management assumes that the name lives on the heap; we
         * copy these names onto the heap here to minimize the complexity of the
         * memory manager. */
        heap_name = calloc(name_len + 1, sizeof(wchar_t));
        wcscpy(heap_name, name);
        err = xl_uri_native(*uri, heap_name);
        return err;
}

no_ignore static xl_error_t
_create_op(
        struct xl_dagc **graph_ptr,
        size_t arity,
        xl_native_evaluator_t evaluator)
{
        struct xl_dagc *graph;
        struct xl_dagc_native *ngraph;
        struct xl_dagc_input *in;
        size_t i;
        xl_error_t err;

        err = xl_alloc_dagc_with_size(&graph, arity + 1, sizeof(struct xl_dagc_native), NULL);
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
        graph->tag |= TAG_GRAPH_NATIVE;
        ngraph->evaluator = evaluator;

        return OK;
}

static xl_error_t
_native_unsigned_add(struct xl_env *env, struct xl_dagc *graph)
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
_register_unsigned_add(struct xl_env *env)
{
        xl_error_t err;

        struct xl_dagc *add_graph;
        struct xl_uri *uri;
        struct xl_value *type;
        union xl_value_or_graph ins;

        add_graph = NULL;
        err = _create_op(&add_graph, 2, _native_unsigned_add);
        if (err != OK)
                return err;
        err = xl_type_word(
                ((struct xl_dagc_input *) add_graph->inputs[0])->required_type);
        if (err != OK)
                return err;
        err = xl_type_word(
                ((struct xl_dagc_input *) add_graph->inputs[1])->required_type);
        if (err != OK)
                return err;

        err = _native_uri(&uri, L"uadd");
        if (err != OK)
                return err;
        err = xl_take(uri);
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

        err = _register_unsigned_add(env);
        if (err != OK)
                return err;

        return OK;
}
