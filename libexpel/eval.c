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

no_ignore static word_t
__eval_apply(struct xl_env *env, struct xl_dagc_apply *node)
{
        unused(env);
        unused(node);
        return ERR_NOT_IMPLEMENTED;
}

no_ignore static word_t
__eval_const(struct xl_env *env, struct xl_dagc_const *node)
{
        unused(env);
        node->head.known_value = node->value;
        node->head.known_type = node->type;
        return OK;
}

no_ignore static word_t
__eval_load(struct xl_env *env, struct xl_dagc_load *node)
{
        struct xl_value *value;
        struct xl_value *type;
        word_t err;

        err = xl_get(&value, &type, env, node->loc);
        if (err != OK)
                return err;

        node->head.known_type = type;
        node->known_value = value;
        return OK;
}

no_ignore static word_t
__eval_store(struct xl_env *env, struct xl_dagc_store *node)
{
        struct xl_value *known_value;
        struct xl_value *known_type;
        word_t err;

        err = xl_dagc_known_value(&known_value, &known_type, node->value);
        if (err != OK)
                return err;

        node->head.known_type = known_type;
        return xl_set(env, node->loc, known_value, known_type);
}

no_ignore word_t
xl_dagc_node_eval(struct xl_env *env, struct xl_dagc_node *node)
{
        word_t err;

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
        default:
                return ERR_UNKNOWN_TYPE;
        }

        if (err == OK)
                node->flags |= XL_DAGC_FLAG_COMPLETE;
        return err;
}
