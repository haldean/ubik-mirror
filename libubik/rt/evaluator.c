/*
 * evaluator.c: evalutes functions in a queue
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

#include <stdlib.h>
#include <string.h>
#include "ubik/alloc.h"
#include "ubik/assert.h"
#include "ubik/rttypes.h"
#include "ubik/ubik.h"

enum node_status
{
        WAIT,
        DONE,
        DATA,
        APPLY,
        NEEDED,
};

struct ubik_evaluator;

struct ubik_eval_state
{
        struct ubik_value *f;
        enum node_status *s;
        struct ubik_value **args;
        struct ubik_value **argtypes;
        struct ubik_value **nv;
        struct ubik_value **nt;
        size_t n;
        size_t term;
};

void
free_eval_state(struct ubik_eval_state *e)
{
        free(e->args);
        free(e->argtypes);
        free(e->nv);
        free(e->nt);
}

no_ignore static ubik_error
ubik_evaluate(struct ubik_value *v, struct ubik_workspace *ws)
{
        struct ubik_value *a;
        struct ubik_value *r;
        struct ubik_node *node;
        struct ubik_eval_state e;
        size_t i;
        size_t t;
        size_t arity;
        enum node_status old_status;
        ubik_error err;

        if (v->type == UBIK_FUN)
                e.f = v;
        else if (v->type == UBIK_PAP)
                e.f = v->pap.base_func;
        e.n = e.f->fun.n;
        arity = e.f->fun.arity;

        ubik_galloc((void **) &e.args, arity, sizeof(struct ubik_value *));
        ubik_galloc((void **) &e.argtypes, arity, sizeof(struct ubik_value *));
        for (i = arity, a = v; i > 0; i--)
        {
                ubik_assert(a->type == UBIK_PAP);;
                e.args[i] = a->pap.arg;
                e.argtypes[i] = a->pap.arg_type;
        }

        ubik_galloc((void **) &e.s, e.n, sizeof(enum node_status));
        ubik_galloc((void **) &e.nv, e.n, sizeof(struct ubik_value *));
        ubik_galloc((void **) &e.nt, e.n, sizeof(struct ubik_value *));

        e.term = 0;
        for (i = 0; i < e.n; i++)
                if (e.f->fun.nodes[i].is_terminal)
                        e.term++;

        memset(e.s, WAIT, e.n * sizeof(enum node_status));

        for (i = 0; i < e.n; i++)
        {
                node = &e.f->fun.nodes[i];
                old_status = e.s[i];
                if (old_status == DONE)
                        continue;
                if (unlikely(node->is_terminal && e.s[i] == WAIT))
                        e.s[i] = NEEDED;

                switch (node->node_type)
                {
                case UBIK_INPUT:
                        t = node->input.arg_num;
                        e.nv[i] = e.args[t];
                        e.nt[i] = e.argtypes[t];
                        e.s[i] = DONE;
                        break;

                case UBIK_REF:
                        t = node->ref.referrent;
                        switch (e.s[t])
                        {
                        case DONE:
                                e.nv[i] = e.nv[t];
                                e.nt[i] = e.nt[t];
                                e.s[i] = DONE;
                                break;
                        case WAIT:
                                if (e.s[i] == NEEDED)
                                        e.s[t] = NEEDED;
                                break;
                        case DATA:
                        case APPLY:
                        case NEEDED:
                                break;
                        }

                case UBIK_VALUE:
                        e.nv[i] = node->value.value;
                        e.nt[i] = node->value.type;
                        e.s[i] = DONE;
                        break;

                case UBIK_COND:
                        t = node->cond.condition;
                        if (e.s[t] == WAIT)
                        {
                                if (e.s[i] == NEEDED)
                                        e.s[t] = NEEDED;
                                break;
                        }
                        if (e.s[t] != DONE)
                                break;

                        ubik_assert(e.nv[t]->type == UBIK_BOO);
                        t = e.nv[t]->boo.value
                                ? node->cond.if_true
                                : node->cond.if_false;
                        if (e.s[t] == WAIT)
                        {
                                if (e.s[i] == NEEDED)
                                        e.s[t] = NEEDED;
                                break;
                        }
                        if (e.s[t] != DONE)
                                break;
                        e.nv[i] = e.args[t];
                        e.nt[i] = e.argtypes[t];
                        e.s[i] = DONE;
                        break;

                case UBIK_APPLY:
                        t = node->apply.func;
                        if (e.s[t] == WAIT && e.s[i] == NEEDED)
                                e.s[t] = NEEDED;
                        t = node->apply.arg;
                        if (e.s[t] == WAIT && e.s[i] == NEEDED)
                                e.s[t] = NEEDED;
                        if (e.s[t] != DONE || e.s[node->apply.func] != DONE)
                                break;

                        err = ubik_value_new(&r, ws);
                        if (err != OK)
                        {
                                free_eval_state(&e);
                                return err;
                        }
                        r->type = UBIK_PAP;
                        r->pap.func = e.nv[node->apply.func];
                        if (r->pap.func->type == UBIK_FUN)
                                r->pap.base_func = r->pap.func;
                        else if (r->pap.func->type == UBIK_PAP)
                                r->pap.base_func = r->pap.func->pap.base_func;
                        r->pap.arg = e.nv[node->apply.arg];
                        r->pap.arg_type = e.nt[node->apply.arg];

                        e.nv[i] = r;

                        err = ubik_value_new(&e.nt[i], ws);
                        if (err != OK)
                        {
                                free_eval_state(&e);
                                return err;
                        }

                        t = 0;
                        a = r;
                        while (a->type == UBIK_PAP)
                        {
                                a = a->pap.func;
                                t++;
                        }
                        if (t == r->pap.base_func->fun.arity)
                        {
                                e.s[i] = APPLY;
                                push_apply(e, r);
                                return OK;
                        }
                        err = ubik_type_func_apply(
                                e.nt[i], e.nt[node->apply.func], e.nt[t]);
                        if (err != OK)
                        {
                                free_eval_state(&e);
                                return err;
                        }
                        e.s[i] = DONE;
                        break;

                case UBIK_LOAD:
                case UBIK_STORE:
                        return ubik_raise(
                                ERR_NOT_IMPLEMENTED,
                                "node evaluation not implemented yet");

                case UBIK_NATIVE:
                default:
                        free_eval_state(&e);
                        return ubik_raise(ERR_BAD_TYPE, "unknown node type");
                }

                if (node->is_terminal && old_status != DONE && e.s[i] == DONE)
                        e.term--;
                if (e.term == 0)
                        break;
        }
}
