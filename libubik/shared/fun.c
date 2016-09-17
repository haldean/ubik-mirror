/*
 * fun.c: utilities for working with function values
 * Copyright (C) 2016, Haldean Brown
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

#include "ubik/fun.h"

void
ubik_fun_from_vector(
        struct ubik_value *res,
        struct ubik_vector *nodes,
        ubik_word result)
{
        ubik_word i;

        res->type = UBIK_FUN;
        res->fun.n = nodes->n;
        res->fun.arity = 0;

        ubik_galloc((void**) &res->fun.nodes,
                    nodes->n, sizeof(struct ubik_node));

        for (i = 0; i < res->fun.n; i++)
        {
                memcpy(&res->fun.nodes[i], nodes->elems[i],
                       sizeof(struct ubik_node));
                if (res->fun.nodes[i].node_type == UBIK_INPUT)
                        res->fun.arity++;
        }

        res->fun.result = result;
        res->fun.nodes[res->fun.result].is_terminal = true;
}

no_ignore ubik_error
ubik_fun_get_deps(
        ubik_word *d1, ubik_word *d2, ubik_word *d3,
        struct ubik_node *n)
{
        switch (n->node_type)
        {
        case UBIK_APPLY:
                *d1 = n->apply.func;
                *d2 = n->apply.arg;
                *d3 = UBIK_INVALID_NODE_ID;
                return OK;
        case UBIK_STORE:
                *d1 = n->store.value;
                *d2 = UBIK_INVALID_NODE_ID;
                *d3 = UBIK_INVALID_NODE_ID;
                return OK;
        case UBIK_REF:
                *d1 = n->ref.referrent;
                *d2 = UBIK_INVALID_NODE_ID;
                *d3 = UBIK_INVALID_NODE_ID;
                return OK;
        case UBIK_COND:
                *d1 = n->cond.condition;
                *d2 = n->cond.if_true;
                *d3 = n->cond.if_false;
                return OK;
        case UBIK_NATIVE:
        case UBIK_LOAD:
        case UBIK_INPUT:
        case UBIK_VALUE:
                *d1 = UBIK_INVALID_NODE_ID;
                *d2 = UBIK_INVALID_NODE_ID;
                *d3 = UBIK_INVALID_NODE_ID;
                return OK;

        case UBIK_MAX_NODE_TYPE:
        default:
                return ubik_raise(ERR_BAD_TYPE, "bad node type in deps");
        }
}

no_ignore ubik_error
ubik_fun_get_parents(
        struct ubik_vector *parents, struct ubik_value *graph, ubik_word node)
{
        ubik_word i;
        ubik_word d1, d2, d3;
        ubik_error err;

        for (i = 0; i < graph->fun.n; i++)
        {
                err = ubik_fun_get_deps(&d1, &d2, &d3, &graph->fun.nodes[i]);
                if (d1 == node || d2 == node || d3 == node)
                {
                        err = ubik_vector_append(
                                parents, (void *) ((uintptr_t) i));
                        if (err != OK)
                                return err;
                }
        }
        return OK;
}
