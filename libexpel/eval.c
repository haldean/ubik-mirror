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

#include "ubik/assert.h"
#include "ubik/dagc.h"
#include "ubik/env.h"
#include "ubik/ubik.h"
#include "ubik/types.h"
#include "ubik/util.h"
#include "ubik/value.h"

no_ignore static xl_error
_eval_apply(struct xl_env *env, struct xl_dagc_apply *node)
{
        xl_error err;
        struct xl_dagc_input *input;
        struct xl_dagc *result;
        struct xl_dagc *proto;
        size_t i;

        unused(env);

        if ((*node->func->known.tag & TAG_TYPE_MASK) != TAG_GRAPH)
        {
                err = xl_type_match_polyfunc(
                        &proto, node->func->known.tree, node->arg->known_type);
                if (err != OK)
                        return err;
        }
        else
        {
                proto = node->func->known.graph;
        }

        if (proto->in_arity == 0)
                return xl_raise(ERR_BAD_TYPE, "apply: graph has no inputs");

        err = xl_dagc_copy(&result, proto);
        if (err != OK)
                return err;
        node->head.known.graph = result;

        err = xl_take(result);
        if (err != OK)
                return err;

        /* Take an input node off the front, shift the remaining ones left. */
        input = (struct xl_dagc_input *) result->inputs[0];
        result->in_arity--;
        for (i = 0; i < result->in_arity; i++)
                result->inputs[i] = result->inputs[i + 1];

        input->head.flags = XL_DAGC_FLAG_COMPLETE;

        input->head.known_type = node->arg->known_type;
        err = xl_take(input->head.known_type);
        if (err != OK)
                return err;

        input->head.known = node->arg->known;
        err = xl_take(input->head.known.any);
        if (err != OK)
                return err;

        err = xl_value_new(&node->head.known_type);
        if (err != OK)
                return err;
        err = xl_type_func_apply(node->head.known_type, node->func->known_type);
        if (err != OK)
                return err;

        node->head.flags |= XL_DAGC_FLAG_COMPLETE;
        return OK;
}

no_ignore static xl_error
_eval_const(struct xl_env *env, struct xl_dagc_const *node)
{
        xl_error err;
        unused(env);

        /* We end up having two references for a single value from a single
         * node; this is so we don't have to worry about whether things are
         * populated when we eventually free the graph. */
        node->head.known_type = node->type;
        err = xl_take(node->type);
        if (err != OK)
                return err;

        node->head.known = node->value;
        err = xl_take(node->head.known.any);
        if (err != OK)
                return err;

        node->head.flags |= XL_DAGC_FLAG_COMPLETE;
        return OK;
}

no_ignore static xl_error
_eval_ref(struct xl_env *env, struct xl_dagc_ref *node)
{
        xl_error err;
        unused(env);

        node->head.known_type = node->referrent->known_type;
        err = xl_take(node->head.known_type);
        if (err != OK)
                return err;

        node->head.known.any = node->referrent->known.any;
        err = xl_take(node->head.known.any);
        if (err != OK)
                return err;

        node->head.flags |= XL_DAGC_FLAG_COMPLETE;
        return OK;
}

no_ignore static xl_error
_mark_load_complete(
        void *node_void,
        struct xl_env *env,
        struct xl_uri *uri)
{
        struct xl_dagc_node *node;
        unused(env);
        unused(uri);

        node = (struct xl_dagc_node *) node_void;
        node->flags &= ~XL_DAGC_FLAG_WAIT_DATA;
        return OK;
}

no_ignore static xl_error
_eval_load(struct xl_env *env, struct xl_dagc_load *node)
{
        union xl_value_or_graph value;
        struct xl_value *type;
        xl_error err;

        err = xl_env_get(&value, &type, env, node->loc);
        if (err != OK)
        {
                /* native funcs never reappear; they're gone forever. */
                if (node->loc->scope == SCOPE_NATIVE)
                        return err;

                if (err->error_code == ERR_ABSENT)
                {
                        free(err);
                        node->head.flags |= XL_DAGC_FLAG_WAIT_DATA;
                        err = xl_env_watch(
                                _mark_load_complete, env, node->loc, node);
                        if (err != OK)
                                return err;
                        return OK;
                }
                return err;
        }

        err = xl_take(value.any);
        if (err != OK)
                return err;

        err = xl_take(type);
        if (err != OK)
                return err;

        node->head.known_type = type;
        node->head.known = value;

        node->head.flags |= XL_DAGC_FLAG_COMPLETE;
        return OK;
}

no_ignore static xl_error
_eval_cond(struct xl_env *env, struct xl_dagc_cond *cond)
{
        struct xl_dagc_node *res;
        xl_error err;
        bool condition;

        unused(env);

        err = xl_value_as_bool(&condition, cond->condition->known.tree);
        if (err != OK)
                return err;

        res = condition ? cond->if_true : cond->if_false;

        if (res->known.any == NULL)
        {
                /* If this is true, we just got done evaluating the condition
                 * but we haven't scheduled the if_true/if_false nodes. We set
                 * our wait flag on the appropriate node and let the scheduler
                 * pick it up and reevaluate us later. */
                cond->head.flags |= condition
                        ? XL_DAGC_FLAG_WAIT_D2
                        : XL_DAGC_FLAG_WAIT_D3;
                return OK;
        }

        cond->head.known_type = res->known_type;
        cond->head.known = res->known;

        err = xl_take(cond->head.known_type);
        if (err != OK)
                return err;
        err = xl_take(cond->head.known.any);
        if (err != OK)
                return err;

        cond->head.flags |= XL_DAGC_FLAG_COMPLETE;
        return OK;
}

no_ignore static xl_error
_eval_store(struct xl_env *env, struct xl_dagc_store *node)
{
        xl_error err;

        node->head.known_type = node->value->known_type;
        node->head.known = node->value->known;

        err = xl_take(node->head.known.any);
        if (err != OK)
                return err;

        err = xl_take(node->head.known_type);
        if (err != OK)
                return err;

        node->head.flags |= XL_DAGC_FLAG_COMPLETE;
        return xl_env_set(
                env, node->loc, node->value->known, node->value->known_type);
}

no_ignore static xl_error
_eval_input(struct xl_env *env, struct xl_dagc_input *node)
{
        unused(env);
        unused(node);

        node->head.flags |= XL_DAGC_FLAG_COMPLETE;
        return OK;
}

no_ignore xl_error
xl_dagc_node_eval(
        struct xl_env *env,
        struct xl_dagc_node *node)
{
        xl_error err;

        xl_assert(!(node->flags & XL_DAGC_WAIT_MASK));

        switch (node->node_type)
        {
        case DAGC_NODE_APPLY:
                err = _eval_apply(env, (struct xl_dagc_apply *) node);
                break;
        case DAGC_NODE_CONST:
                err = _eval_const(env, (struct xl_dagc_const *) node);
                break;
        case DAGC_NODE_LOAD:
                err = _eval_load(env, (struct xl_dagc_load *) node);
                break;
        case DAGC_NODE_STORE:
                err = _eval_store(env, (struct xl_dagc_store *) node);
                break;
        case DAGC_NODE_INPUT:
                err = _eval_input(env, (struct xl_dagc_input *) node);
                break;
        case DAGC_NODE_COND:
                err = _eval_cond(env, (struct xl_dagc_cond *) node);
                break;
        case DAGC_NODE_REF:
                err = _eval_ref(env, (struct xl_dagc_ref *) node);
                break;
        case DAGC_NODE_NATIVE:
                return xl_raise(ERR_BAD_TYPE, "node_eval: can't eval native");
        default:
                return xl_raise(ERR_UNKNOWN_TYPE, "node_eval");
        }

        if (err != OK)
                return err;

        return OK;
}
