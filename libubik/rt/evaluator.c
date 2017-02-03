/*
 * evaluator.c: evalutes functions in a queue
 * Copyright (C) 2017, Haldean Brown
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
#include "ubik/deque.h"
#include "ubik/evaluator.h"
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

struct ubik_evaluator
{
        struct ubik_deque q;
        struct ubik_workspace *ws;
};

struct ubik_eval_req
{
        struct ubik_eval_state *e;
        struct ubik_eval_state *waiting;
        size_t node;
};

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
push(
        struct ubik_evaluator *evaluator,
        struct ubik_value *v,
        struct ubik_eval_state *waiting,
        size_t wait_node);

no_ignore static ubik_error
run_state(
        struct ubik_evaluator *evaluator,
        struct ubik_eval_state *e)
{
        struct ubik_value *a;
        struct ubik_value *r;
        struct ubik_node *node;
        size_t i;
        size_t t;
        enum node_status old_status;
        ubik_error err;

        for (i = 0; i < e->n; i++)
        {
                node = &e->f->fun.nodes[i];
                old_status = e->s[i];
                if (old_status == DONE)
                        continue;
                if (unlikely(node->is_terminal && e->s[i] == WAIT))
                        e->s[i] = NEEDED;

                switch (node->node_type)
                {
                case UBIK_INPUT:
                        t = node->input.arg_num;
                        e->nv[i] = e->args[t];
                        e->nt[i] = e->argtypes[t];
                        e->s[i] = DONE;
                        break;

                case UBIK_REF:
                        t = node->ref.referrent;
                        switch (e->s[t])
                        {
                        case DONE:
                                e->nv[i] = e->nv[t];
                                e->nt[i] = e->nt[t];
                                e->s[i] = DONE;
                                break;
                        case WAIT:
                                if (e->s[i] == NEEDED)
                                        e->s[t] = NEEDED;
                                break;
                        case DATA:
                        case APPLY:
                        case NEEDED:
                                break;
                        }

                case UBIK_VALUE:
                        e->nv[i] = node->value.value;
                        e->nt[i] = node->value.type;
                        e->s[i] = DONE;
                        break;

                case UBIK_COND:
                        t = node->cond.condition;
                        if (e->s[t] == WAIT)
                        {
                                if (e->s[i] == NEEDED)
                                        e->s[t] = NEEDED;
                                break;
                        }
                        if (e->s[t] != DONE)
                                break;

                        ubik_assert(e->nv[t]->type == UBIK_BOO);
                        t = e->nv[t]->boo.value
                                ? node->cond.if_true
                                : node->cond.if_false;
                        if (e->s[t] == WAIT)
                        {
                                if (e->s[i] == NEEDED)
                                        e->s[t] = NEEDED;
                                break;
                        }
                        if (e->s[t] != DONE)
                                break;
                        e->nv[i] = e->args[t];
                        e->nt[i] = e->argtypes[t];
                        e->s[i] = DONE;
                        break;

                case UBIK_APPLY:
                        t = node->apply.func;
                        if (e->s[t] == WAIT && e->s[i] == NEEDED)
                                e->s[t] = NEEDED;
                        t = node->apply.arg;
                        if (e->s[t] == WAIT && e->s[i] == NEEDED)
                                e->s[t] = NEEDED;
                        if (e->s[t] != DONE || e->s[node->apply.func] != DONE)
                                break;

                        err = ubik_value_new(&r, evaluator->ws);
                        if (err != OK)
                                return err;

                        r->type = UBIK_PAP;
                        r->pap.func = e->nv[node->apply.func];
                        if (r->pap.func->type == UBIK_FUN)
                                r->pap.base_func = r->pap.func;
                        else if (r->pap.func->type == UBIK_PAP)
                                r->pap.base_func = r->pap.func->pap.base_func;
                        r->pap.arg = e->nv[node->apply.arg];
                        r->pap.arg_type = e->nt[node->apply.arg];

                        e->nv[i] = r;

                        err = ubik_value_new(&e->nt[i], evaluator->ws);
                        if (err != OK)
                                return err;

                        t = 0;
                        a = r;
                        while (a->type == UBIK_PAP)
                        {
                                a = a->pap.func;
                                t++;
                        }
                        if (t == r->pap.base_func->fun.arity)
                        {
                                e->s[i] = APPLY;
                                err = push(evaluator, r, e, i);
                                if (err != OK)
                                        return err;
                                break;
                        }
                        err = ubik_type_func_apply(
                                e->nt[i], e->nt[node->apply.func], e->nt[t]);
                        if (err != OK)
                                return err;
                        e->s[i] = DONE;
                        break;

                case UBIK_LOAD:
                case UBIK_STORE:
                        return ubik_raise(
                                ERR_NOT_IMPLEMENTED,
                                "node evaluation not implemented yet");

                case UBIK_NATIVE:
                case UBIK_MAX_NODE_TYPE:
                default:
                        return ubik_raise(ERR_BAD_TYPE, "unknown node type");
                }

                if (node->is_terminal && old_status != DONE && e->s[i] == DONE)
                        e->term--;
                if (e->term == 0)
                        break;
        }

        return OK;
}

no_ignore static ubik_error
push(
        struct ubik_evaluator *evaluator,
        struct ubik_value *v,
        struct ubik_eval_state *waiting,
        size_t wait_node)
{
        struct ubik_value *a;
        struct ubik_eval_req *req;
        struct ubik_eval_state *e;
        size_t i;
        size_t arity;

        ubik_galloc((void **) &req, 1, sizeof(struct ubik_eval_req));
        ubik_galloc((void **) &e, 1, sizeof(struct ubik_eval_state));
        req->e = e;
        req->waiting = waiting;
        req->node = wait_node;

        if (v->type == UBIK_FUN)
                e->f = v;
        else if (v->type == UBIK_PAP)
                e->f = v->pap.base_func;
        e->n = e->f->fun.n;
        arity = e->f->fun.arity;

        ubik_galloc((void **) &e->args, arity, sizeof(struct ubik_value *));
        ubik_galloc((void **) &e->argtypes, arity, sizeof(struct ubik_value *));
        for (i = arity, a = v; i > 0; i--)
        {
                ubik_assert(a->type == UBIK_PAP);;
                e->args[i] = a->pap.arg;
                e->argtypes[i] = a->pap.arg_type;
        }

        ubik_galloc((void **) &e->s, e->n, sizeof(enum node_status));
        ubik_galloc((void **) &e->nv, e->n, sizeof(struct ubik_value *));
        ubik_galloc((void **) &e->nt, e->n, sizeof(struct ubik_value *));

        e->term = 0;
        for (i = 0; i < e->n; i++)
                if (e->f->fun.nodes[i].is_terminal)
                        e->term++;

        memset(e->s, WAIT, e->n * sizeof(enum node_status));

        ubik_deque_pushl(&evaluator->q, req);
        return OK;
}

no_ignore ubik_error
ubik_evaluate_push(
        struct ubik_evaluator *evaluator,
        struct ubik_value *v)
{
        return push(evaluator, v, NULL, 0);
}

no_ignore ubik_error
ubik_evaluate_push_roots(
        struct ubik_evaluator *evaluator,
        struct ubik_workspace *ws)
{
        ubik_error err;
        size_t i;

        for (; ws != NULL; ws = ws->next)
        {
                for (i = 0; i < ws->n; i++)
                {
                        if (!ws->values[i].gc.root)
                                continue;
                        err = push(evaluator, &ws->values[i], NULL, 0);
                        if (err != OK)
                                return err;
                }
        }

        return OK;
}

no_ignore ubik_error
ubik_evaluate_run(struct ubik_evaluator *evaluator)
{
        struct ubik_eval_req *r;
        ubik_error err;
        size_t t0;
        size_t t1;

        while (!ubik_deque_empty(&evaluator->q))
        {
                r = ubik_deque_popr(&evaluator->q);

                err = run_state(evaluator, r->e);
                if (err != OK)
                        return err;
                if (r->e->term != 0)
                        ubik_deque_pushl(&evaluator->q, r);
                else
                {
                        t0 = r->node;
                        t1 = r->e->f->fun.result;
                        r->waiting->nv[t0] = r->e->nv[t1];
                        r->waiting->nt[t0] = r->e->nt[t1];
                        free_eval_state(r->e);
                        free(r->e);
                        free(r);
                }
        }

        return OK;
}

no_ignore ubik_error
ubik_evaluate_new(struct ubik_evaluator **evaluator)
{
        ubik_galloc((void **) evaluator, 1, sizeof(struct ubik_evaluator));
        return OK;
}

void
ubik_evaluate_free(struct ubik_evaluator *evaluator)
{
        while (!ubik_deque_empty(&evaluator->q))
                ubik_deque_popl(&evaluator->q);
        free(evaluator);
}
