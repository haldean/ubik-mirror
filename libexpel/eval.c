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
#include "expel/value.h"

no_ignore static xl_error_t
__eval_apply(struct xl_env *env, struct xl_dagc_apply *node)
{
        xl_error_t err;

        unused(env);

        err = xl_dagc_apply_arg(
                &node->head.known.graph, node->func->known.graph, node->arg);
        if (err != OK)
                return err;

        err = xl_take(node->head.known.graph);
        if (err != OK)
                return err;

        node->head.value_type = DAGC_TYPE_GRAPH;
        return OK;
}

no_ignore static xl_error_t
__eval_const(struct xl_env *env, struct xl_dagc_const *node)
{
        xl_error_t err;
        unused(env);

        /* We end up having two references for a single value from a single
         * node; this is so we don't have to worry about whether things are
         * populated when we eventually free the graph. */
        node->head.known_type = node->type;
        err = xl_take(node->type);
        if (err != OK)
                return err;

        /* Aren't unions just grand? */
        err = xl_take(node->value.any);
        if (err != OK)
                return err;

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

        /* Release the reference we took to the value that we don't actually
         * want anymore, and swallow any errors so we can return the error that
         * people actually care about. */
        err = xl_release(node->value.any);
        unused(err);

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

        err = xl_take(value.any);
        if (err != OK)
                return err;

        err = xl_take(type);
        if (err != OK)
                return err;

        node->head.value_type = value_type;
        node->head.known_type = type;
        node->head.known = value;
        return OK;
}

no_ignore static xl_error_t
__eval_cond(struct xl_env *env, struct xl_dagc_cond *cond)
{
        struct xl_dagc_node *res;
        xl_error_t err;
        bool condition;

        unused(env);

        err = xl_value_as_bool(&condition, cond->condition->known.tree);
        if (err != OK)
                return err;

        res = condition ? cond->if_true : cond->if_false;
        cond->head.value_type = res->value_type;
        cond->head.known_type = res->known_type;
        cond->head.known = res->known;

        err = xl_take(cond->head.known_type);
        if (err != OK)
                return err;
        err = xl_take(cond->head.known.any);
        if (err != OK)
                return err;
        return OK;
}

no_ignore static xl_error_t
__eval_store(struct xl_env *env, struct xl_dagc_store *node)
{
        xl_error_t err;

        node->head.value_type = node->value->value_type;
        node->head.known_type = node->value->known_type;
        node->head.known = node->value->known;

        err = xl_take(node->head.known.any);
        if (err != OK)
                return err;

        err = xl_take(node->head.known_type);
        if (err != OK)
                return err;

        return xl_set(
                env, node->loc, node->value->known, node->value->known_type,
                node->value->value_type);
}

no_ignore static xl_error_t
__eval_input(struct xl_env *env, struct xl_dagc_input *node)
{
        unused(env);
        unused(node);
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
        case DAGC_NODE_COND:
                err = __eval_cond(env, (struct xl_dagc_cond *) node);
                break;
        case DAGC_NODE_NATIVE:
                return xl_raise(ERR_BAD_TYPE, "node_eval: can't eval native");
        default:
                return xl_raise(ERR_UNKNOWN_TYPE, "node_eval");
        }

        if (err != OK)
                return err;

        node->flags |= XL_DAGC_FLAG_COMPLETE;

        err = xl_dagc_collapse_graph(node, env);
        return err;
}
