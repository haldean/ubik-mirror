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

#include "expel/assert.h"
#include "expel/dagc.h"
#include "expel/env.h"
#include "expel/expel.h"
#include "expel/util.h"

no_ignore static xl_error_t
__eval_apply(struct xl_env *env, struct xl_dagc_apply *node)
{
        unused(env);
        unused(node);
        return xl_raise(ERR_NOT_IMPLEMENTED, "eval_apply");
}

no_ignore static xl_error_t
__eval_const(struct xl_env *env, struct xl_dagc_const *node)
{
        unused(env);
        node->head.known_type = node->type;
        if (node->const_type == DAGC_CONST_VALUE)
        {
                node->head.known_value = node->value.tree;
                node->head.known_graph = NULL;
                return OK;
        }
        if (node->const_type == DAGC_CONST_GRAPH)
        {
                node->head.known_value = NULL;
                node->head.known_graph = node->value.graph;
                return OK;
        }
        return xl_raise(ERR_BAD_TYPE, "eval_const subtype");
}

no_ignore static xl_error_t
__eval_load(struct xl_env *env, struct xl_dagc_load *node)
{
        struct xl_value *value;
        struct xl_value *type;
        xl_error_t err;

        err = xl_get(&value, &type, env, node->loc);
        if (err != OK)
                return err;

        node->head.known_type = type;
        node->head.known_value = value;
        return OK;
}

no_ignore static xl_error_t
__eval_store(struct xl_env *env, struct xl_dagc_store *node)
{
        struct xl_value *known_value;
        struct xl_value *known_type;
        xl_error_t err;

        err = xl_dagc_known_value(&known_value, &known_type, node->value);
        if (err != OK)
                return err;

        node->head.known_type = known_type;
        return xl_set(env, node->loc, known_value, known_type);
}

no_ignore static xl_error_t
__eval_input(struct xl_env *env, struct xl_dagc_input *node)
{
        unused(env);
        node->head.known_value = node->applied_value;
        node->head.known_type = node->applied_type;
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
