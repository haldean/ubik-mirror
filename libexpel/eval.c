/*
 * eval.c: evaluate nodes in directed acyclic graphs of computation
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

#include "expel/apply.h"
#include "expel/assert.h"
#include "expel/dagc.h"
#include "expel/env.h"
#include "expel/expel.h"
#include "expel/util.h"

no_ignore static xl_error_t
__eval_apply(struct xl_env *env, struct xl_dagc_apply *node)
{
        struct xl_dagc *graph;
        xl_error_t err;
        unused(env);

        graph = calloc(1, sizeof(struct xl_dagc));
        if (graph == NULL)
                return xl_raise(ERR_NO_MEMORY, "eval apply: graph alloc");

        if (node->func->value_type != DAGC_TYPE_GRAPH)
                return xl_raise(
                        ERR_BAD_TYPE, "eval apply: func node is not a graph");

        err = xl_dagc_apply_arg(graph, node->func->known.graph, node->arg);
        if (err != OK)
                return err;

        if (graph->in_arity != 0)
        {
                node->head.known.graph = graph;
                node->head.value_type = DAGC_TYPE_GRAPH;
                return OK;
        }

        /* Graph is fully applied; we can evaluate it to find the value of this
         * node. */
        if (graph->out_arity != 1)
                return xl_raise(
                        ERR_BAD_TYPE,
                        "eval apply: can't call graph with multiple terminals");
        err = xl_dagc_eval(env, graph);
        if (err != OK)
                return err;

        node->head.value_type = graph->terminals[0]->value_type;
        node->head.known = graph->terminals[0]->known;
        return OK;
}

no_ignore static xl_error_t
__eval_const(struct xl_env *env, struct xl_dagc_const *node)
{
        unused(env);
        node->head.known_type = node->type;
        if (node->head.value_type == DAGC_TYPE_VALUE)
        {
                node->head.known.tree = node->value.tree;
                return OK;
        }
        if (node->head.value_type == DAGC_TYPE_GRAPH)
        {
                node->head.known.graph = node->value.graph;
                return OK;
        }
        return xl_raise(ERR_BAD_TYPE, "eval_const subtype");
}

no_ignore static xl_error_t
__eval_load(struct xl_env *env, struct xl_dagc_load *node)
{
        union xl_value_or_graph value;
        struct xl_value *type;
        word_t value_type;
        xl_error_t err;

        err = xl_get(&value, &type, &value_type, env, node->loc);
        if (err != OK)
                return err;

        node->head.value_type = value_type;
        node->head.known_type = type;
        node->head.known = value;
        return OK;
}

no_ignore static xl_error_t
__eval_store(struct xl_env *env, struct xl_dagc_store *node)
{
        node->head.value_type = node->value->value_type;
        node->head.known_type = node->value->known_type;
        node->head.known = node->value->known;

        return xl_set(
                env, node->loc, node->value->known, node->value->known_type,
                node->value->value_type);
}

no_ignore static xl_error_t
__eval_input(struct xl_env *env, struct xl_dagc_input *node)
{
        unused(env);
        node->head.known_type = node->applied_type;
        node->head.known = node->applied;
        return OK;
}

no_ignore xl_error_t
xl_dagc_node_eval(struct xl_env *env, struct xl_dagc_node *node)
{
        xl_error_t err;

        xl_assert((node->flags & XL_DAGC_READY_MASK) == XL_DAGC_READY_MASK);

        switch (node->node_type)
        {
        case DAGC_NODE_APPLY:
                err = __eval_apply(env, (struct xl_dagc_apply *) node);
                break;
        case DAGC_NODE_CONST:
                err = __eval_const(env, (struct xl_dagc_const *) node);
                break;
        case DAGC_NODE_LOAD:
                err = __eval_load(env, (struct xl_dagc_load *) node);
                break;
        case DAGC_NODE_STORE:
                err = __eval_store(env, (struct xl_dagc_store *) node);
                break;
        case DAGC_NODE_INPUT:
                err = __eval_input(env, (struct xl_dagc_input *) node);
                break;
        default:
                return xl_raise(ERR_UNKNOWN_TYPE, "node_eval");
        }

        if (err == OK)
                node->flags |= XL_DAGC_FLAG_COMPLETE;
        return err;
}
