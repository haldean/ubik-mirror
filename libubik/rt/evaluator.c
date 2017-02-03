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
#include "ubik/ubik.h"

enum node_status
{
        WAIT,
        DONE,
        DATA,
        APPLY,
};

struct ubik_evaluator;

no_ignore static ubik_error
ubik_evaluate(struct ubik_evaluator *ev, struct ubik_value *v)
{
        enum node_status *s;
        struct ubik_value *f;
        struct ubik_value *a;
        struct ubik_value **args;
        struct ubik_value **argtypes;
        struct ubik_value **nv;
        struct ubik_value **nt;
        struct ubik_node *node;
        size_t i;
        size_t n;
        size_t arity;

        if (v->type == UBIK_FUN)
                f = v;
        else if (v->type == UBIK_PAP)
                f = v->pap.base_func;
        n = f->fun.n;
        arity = f->fun.arity;

        ubik_galloc((void **) &args, arity, sizeof(struct ubik_value *));
        ubik_galloc((void **) &argtypes, arity, sizeof(struct ubik_value *));
        for (i = arity, a = v; i > 0; i--)
        {
                ubik_assert(a->type == UBIK_PAP);;
                args[i] = a->pap.arg;
                argtypes[i] = a->pap.arg_type;
        }

        ubik_galloc((void **) &s, n, sizeof(enum node_status));
        ubik_galloc((void **) &nv, n, sizeof(struct ubik_value *));
        ubik_galloc((void **) &nt, n, sizeof(struct ubik_value *));

        memset(s, WAIT, n * sizeof(enum node_status));
        for (i = 0; i < n; i++)
        {
                node = &f->fun.nodes[i];
                switch (node->node_type)
                {
                case UBIK_INPUT:
                        nv[i] = args[node->input.arg_num];
                        nt[i] = argtypes[node->input.arg_num];
                        s[i] = DONE;
                        break;

                case UBIK_REF:
                        nv[i] = nv[node->ref.referrent];
                        nt[i] = nt[node->ref.referrent];
                        s[i] = DONE;
                        break;

                case UBIK_VALUE:
                        nv[i] = node->value.value;
                        nt[i] = node->value.type;
                        s[i] = DONE;
                        break;

                case UBIK_APPLY:
                case UBIK_COND:
                case UBIK_LOAD:
                case UBIK_STORE:
                        return ubik_raise(
                                ERR_NOT_IMPLEMENTED,
                                "node evaluation not implemented yet");
                case UBIK_NATIVE:
                default:
                        return ubik_raise(ERR_BAD_TYPE, "unknown node type");
                }
        }
}
