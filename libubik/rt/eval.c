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
#include "ubik/env.h"
#include "ubik/rttypes.h"
#include "ubik/schedule.h"
#include "ubik/ubik.h"
#include "ubik/util.h"
#include "ubik/value.h"

no_ignore static ubik_error
_eval_apply(struct ubik_exec_unit *u, struct ubik_node *n)
{
        ubik_error err;
        struct ubik_value *res;
        struct ubik_value *type;

        err = ubik_value_new(&res, u->gexec->workspace);
        if (err != OK)
                return err;
        err = ubik_value_new(&type, u->gexec->workspace);
        if (err != OK)
                return err;

        res->type = UBIK_PAP;
        res->pap.func = u->gexec->nv[n->apply.func];
        res->pap.arg = u->gexec->nv[n->apply.arg];
        res->pap.arg_type = u->gexec->nt[n->apply.arg];

        err = ubik_type_func_apply(
                type, u->gexec->nt[n->apply.func], u->gexec->nt[n->apply.arg]);
        if (err != OK)
                return err;

        u->gexec->nv[n->id] = res;
        u->gexec->nt[n->id] = type;
        u->gexec->status[n->id] = UBIK_STATUS_COMPLETE;
        return OK;
}

no_ignore static ubik_error
_eval_value(struct ubik_exec_unit *u, struct ubik_node *n)
{
        u->gexec->nv[n->id] = n->value.value;
        u->gexec->nt[n->id] = n->value.type;
        u->gexec->status[n->id] = UBIK_STATUS_COMPLETE;
        return OK;
}

no_ignore static ubik_error
_eval_ref(struct ubik_exec_unit *u, struct ubik_node *n)
{
        u->gexec->nv[n->id] = u->gexec->nv[n->ref.referrent];
        u->gexec->nt[n->id] = u->gexec->nt[n->ref.referrent];
        u->gexec->status[n->id] = UBIK_STATUS_COMPLETE;
        return OK;
}

no_ignore static ubik_error
_mark_load_complete(
        void *unit_void,
        struct ubik_env *env,
        struct ubik_uri *uri)
{
        struct ubik_exec_unit *u;
        unused(env);
        unused(uri);

        u = unit_void;
        u->gexec->status[u->node] &= ~UBIK_STATUS_WAIT_DATA;
        return OK;
}

no_ignore static ubik_error
_eval_load(struct ubik_exec_unit *u, struct ubik_node *n)
{
        struct ubik_value *value;
        struct ubik_value *type;
        ubik_error err;

        err = ubik_env_get(&value, &type, u->gexec->env, n->load.loc);
        if (err != OK)
        {
                /* native funcs never reappear; they're gone forever. */
                if (n->load.loc->scope == SCOPE_NATIVE)
                {
                        if (err->error_code == ERR_ABSENT)
                        {
                                char *buf = ubik_uri_explain(n->load.loc);
                                printf("tried to access nonexistent native "
                                       "function %s\n", buf);
                                free(buf);
                        }
                        return err;
                }

                if (err->error_code == ERR_ABSENT)
                {
                        free(err);
                        u->gexec->status[n->id] |= UBIK_STATUS_WAIT_DATA;
                        err = ubik_env_watch(
                                _mark_load_complete, u->gexec->env,
                                n->load.loc, u);
                        if (err != OK)
                                return err;
                        return OK;
                }
                return err;
        }

        u->gexec->nv[n->id] = value;
        u->gexec->nt[n->id] = type;
        u->gexec->status[n->id] = UBIK_STATUS_COMPLETE;
        return OK;
}

no_ignore static ubik_error
_eval_cond(struct ubik_exec_unit *u, struct ubik_node *n)
{
        ubik_word res;
        bool condition;

        ubik_assert(u->gexec->nv[n->cond.condition]->type == UBIK_BOO);
        condition = u->gexec->nv[n->cond.condition]->boo.value;
        res = condition ? n->cond.if_true : n->cond.if_false;

        if (u->gexec->status[res] != UBIK_STATUS_COMPLETE)
        {
                /* If this is true, we just got done evaluating the condition
                 * but we haven't scheduled the if_true/if_false nodes. We set
                 * our wait flag on the appropriate node and let the scheduler
                 * pick it up and reevaluate us later. */
                u->gexec->status[n->id] |= condition
                        ? UBIK_STATUS_WAIT_D2
                        : UBIK_STATUS_WAIT_D3;
                return OK;
        }

        u->gexec->nv[n->id] = u->gexec->nv[res];
        u->gexec->nt[n->id] = u->gexec->nt[res];
        u->gexec->status[n->id] = UBIK_STATUS_COMPLETE;
        return OK;
}

no_ignore static ubik_error
_eval_store(struct ubik_exec_unit *u, struct ubik_node *n)
{
        u->gexec->nv[n->id] = u->gexec->nv[n->store.value];
        u->gexec->nt[n->id] = u->gexec->nt[n->store.value];
        u->gexec->status[n->id] = UBIK_STATUS_COMPLETE;

        return ubik_env_set(
                u->gexec->env, n->store.loc,
                u->gexec->nv[n->id], u->gexec->nt[n->id]);
}

no_ignore static ubik_error
_eval_input(struct ubik_exec_unit *u, struct ubik_node *n)
{
        ubik_word arity;
        struct ubik_value *t;

        /* find the arity of the original function */
        t = u->gexec->v;
        for (t = u->gexec->v, arity = 0;
             t->type == UBIK_PAP;
             arity++, t = t->pap.func);

        /* ...then traverse back to the PAP corresponding to the application of
           the argument we're evaluating. */
        for (arity = arity - n->input.arg_num - 1, t = u->gexec->v;
             arity > 0; arity--, t = t->pap.func);

        u->gexec->nv[n->id] = t->pap.arg;
        u->gexec->nt[n->id] = t->pap.arg_type;
        u->gexec->status[n->id] = UBIK_STATUS_COMPLETE;
        return OK;
}

no_ignore ubik_error
ubik_node_eval(struct ubik_exec_unit *u)
{
        struct ubik_node *n;
        struct ubik_value *fun;
        ubik_error err;

        ubik_assert(!(u->gexec->status[u->node] & UBIK_STATUS_WAIT_MASK));

        fun = u->gexec->v;
        while (fun->type == UBIK_PAP)
                fun = fun->pap.func;
        ubik_assert(fun->type == UBIK_FUN);
        n = &fun->fun.nodes[u->node];

        switch (n->node_type)
        {
        case UBIK_APPLY:
                err = _eval_apply(u, n);
                break;
        case UBIK_VALUE:
                err = _eval_value(u, n);
                break;
        case UBIK_LOAD:
                err = _eval_load(u, n);
                break;
        case UBIK_STORE:
                err = _eval_store(u, n);
                break;
        case UBIK_INPUT:
                err = _eval_input(u, n);
                break;
        case UBIK_COND:
                err = _eval_cond(u, n);
                break;
        case UBIK_REF:
                err = _eval_ref(u, n);
                break;
        case UBIK_NATIVE:
                return ubik_raise(ERR_BAD_TYPE, "node_eval: can't eval native");
        case UBIK_MAX_NODE_TYPE:
        default:
                return ubik_raise(ERR_UNKNOWN_TYPE, "node_eval");
        }

        if (err != OK)
                return err;

        return OK;
}
