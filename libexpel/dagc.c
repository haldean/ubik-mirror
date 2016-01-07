/*
 * dagc.c: common tasks for directed acyclic graphs of computation
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

#include <stdbool.h>
#include <stdlib.h>

#include "expel/dagc.h"

struct __xl_dagc_adjacency
{
        struct xl_dagc_node *child;
        struct xl_dagc_node **parents;
        size_t n_parents;
};

/* Gets the dependencies of a node.
 *
 * For nodes with two dependencies, d1 and d2 will be filled in
 * with valid pointers. For nodes with one dependency, d1 will be
 * filled in with a pointer and d2 will be set to NULL. For nodes
 * with no dependencies, both will be NULL. */
no_ignore xl_error_t
xl_dagc_get_deps(
                struct xl_dagc_node **d1,
                struct xl_dagc_node **d2,
                struct xl_dagc_node *n)
{
        switch (n->node_type)
        {
        case DAGC_NODE_APPLY:
                *d1 = ((struct xl_dagc_apply *) n)->func;
                *d2 = ((struct xl_dagc_apply *) n)->arg;
                return OK;
        case DAGC_NODE_LOAD:
                *d1 = ((struct xl_dagc_load *) n)->dependent_store;
                *d2 = NULL;
                return OK;
        case DAGC_NODE_STORE:
                *d1 = ((struct xl_dagc_store *) n)->value;
                *d2 = NULL;
                return OK;
        case DAGC_NODE_CONST:
        case DAGC_NODE_DISPATCH:
        case DAGC_NODE_INPUT:
                *d1 = NULL;
                *d2 = NULL;
                return OK;
        default:
                return xl_raise(ERR_UNKNOWN_TYPE, "get deps");
        }
}

static int
__cmp_adjacency(const void *v1, const void *v2)
{
        uintptr_t p1, p2;
        p1 = *((uintptr_t *) v1);
        p2 = *((uintptr_t *) v2);
        if (p1 < p2)
                return -1;
        if (p1 > p2)
                return 1;
        return 0;
}

static no_ignore xl_error_t
__find_adjacency(
        size_t *i,
        struct __xl_dagc_adjacency *adjacencies,
        size_t n,
        struct xl_dagc_node *child)
{
        uintptr_t cptr, aptr;
        size_t min, max;

        min = 0;
        max = n;
        cptr = (uintptr_t) child;

        while (min < max)
        {
                *i = (min + max - 1) / 2;
                aptr = (uintptr_t) adjacencies[*i].child;
                if (aptr == cptr)
                        return OK;
                if (aptr > cptr)
                        max = *i;
                if (aptr < cptr)
                        min = *i + 1;
        }
        return xl_raise(ERR_ABSENT, "find adjacency");
}

no_ignore xl_error_t
xl_dagc_init(struct xl_dagc *graph)
{
        struct xl_dagc_node *p, *c1, *c2;
        struct __xl_dagc_adjacency *adj;
        size_t i, j, a, next_in, next_out;
        xl_error_t err;

        /* Adjacency is stored as a sorted list of adjacency
         * lists; the first element in each list is the child and
         * the remaining elements are parents. */
        graph->adjacency = calloc(graph->n, sizeof(struct __xl_dagc_adjacency));

        for (i = 0; i < graph->n; i++)
        {
                graph->adjacency[i].child = graph->nodes[i];
                graph->adjacency[i].parents = NULL;
                graph->adjacency[i].n_parents = 0;
        }

        qsort(graph->adjacency, graph->n,
              sizeof(struct __xl_dagc_adjacency), __cmp_adjacency);

        /* First go through and count how many parents each one has. */
        for (i = 0; i < graph->n; i++)
        {
                p = graph->nodes[i];
                err = xl_dagc_get_deps(&c1, &c2, p);
                if (err != OK)
                        return err;

                if (c1 != NULL)
                {
                        err = __find_adjacency(
                                &a, graph->adjacency, graph->n, c1);
                        if (err != OK)
                                return err;
                        graph->adjacency[a].n_parents++;
                }
                if (c2 != NULL)
                {
                        err = __find_adjacency(
                                &a, graph->adjacency, graph->n, c2);
                        if (err != OK)
                                return err;
                        graph->adjacency[a].n_parents++;
                }
        }

        /* Now allocate the parent arrays. */
        for (i = 0; i < graph->n; i++)
        {
                adj = &graph->adjacency[i];
                adj->parents = calloc(
                        adj->n_parents, sizeof(struct xl_dagc_node *));
        }

        /* Finally fill in the actual parent arrays. */
        for (i = 0; i < graph->n; i++)
        {
                p = graph->nodes[i];
                err = xl_dagc_get_deps(&c1, &c2, p);
                if (err != OK)
                        return err;

                if (c1 != NULL)
                {
                        err = __find_adjacency(
                                &a, graph->adjacency, graph->n, c1);
                        if (err != OK)
                                return err;
                        adj = &graph->adjacency[a];
                        /* Find the first NULL parent entry. */
                        for (j = 0; adj->parents[j] && j < adj->n_parents; j++);
                        adj->parents[j] = p;
                }
                if (c2 != NULL)
                {
                        err = __find_adjacency(
                                &a, graph->adjacency, graph->n, c2);
                        if (err != OK)
                                return err;
                        adj = &graph->adjacency[a];
                        for (j = 0; adj->parents[j] && j < adj->n_parents; j++);
                        adj->parents[j] = p;
                }
        }

        /* Go through and find how many nodes are inputs or terminals */
        graph->in_arity = 0;
        graph->out_arity = 0;
        for (i = 0; i < graph->n; i++)
        {
                p = graph->nodes[i];
                if (p->node_type == DAGC_NODE_INPUT)
                        graph->in_arity++;
                if (p->is_terminal)
                        graph->out_arity++;
        }

        /* Then populate the input and terminal lists. */
        graph->inputs = calloc(graph->in_arity, sizeof(struct xl_dagc_node *));
        graph->terminals =
                calloc(graph->out_arity, sizeof(struct xl_dagc_node *));
        for (i = 0, next_in = 0, next_out = 0; i < graph->n; i++)
        {
                p = graph->nodes[i];
                if (p->node_type == DAGC_NODE_INPUT)
                        graph->inputs[next_in++] = p;
                if (p->is_terminal)
                        graph->terminals[next_out++] = p;
        }

        return OK;
}

no_ignore xl_error_t
xl_dagc_known_value(
        struct xl_value **value,
        struct xl_value **type,
        struct xl_dagc_node *node)
{
        struct xl_dagc_const *c;

        *type = node->known_type;

        switch (node->node_type)
        {
        case DAGC_NODE_CONST:
                c = (struct xl_dagc_const *) node;
                if (c->const_type == DAGC_CONST_VALUE)
                {
                        *value = node->known_value;
                        return OK;
                }
                return xl_raise(ERR_BAD_TYPE, "const known value");
        case DAGC_NODE_APPLY:
        case DAGC_NODE_LOAD:
        case DAGC_NODE_INPUT:
                *value = node->known_value;
                return OK;
        }
        return xl_raise(ERR_BAD_TYPE, "known value");
}

no_ignore xl_error_t
xl_dagc_get_parents(
        struct xl_dagc_node ***parents,
        size_t *n_parents,
        struct xl_dagc *graph,
        struct xl_dagc_node *child)
{
        size_t i;
        xl_error_t err;

        err = __find_adjacency(&i, graph->adjacency, graph->n, child);
        if (err != OK)
                return err;

        *parents = graph->adjacency[i].parents;
        *n_parents = graph->adjacency[i].n_parents;
        return OK;
}
