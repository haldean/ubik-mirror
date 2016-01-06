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
no_ignore word_t
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
        case DAGC_NODE_CONST:
                *d1 = NULL;
                *d2 = NULL;
                return OK;
        case DAGC_NODE_LOAD:
                *d1 = NULL;
                *d2 = NULL;
                return OK;
        case DAGC_NODE_STORE:
                *d1 = ((struct xl_dagc_store *) n)->value;
                *d2 = NULL;
                return OK;
        default:
                return ERR_UNKNOWN_TYPE;
        }
}

static int
__cmp_adjacency(const void *v1, const void *v2)
{
        uintptr_t *p1, *p2;
        p1 = (uintptr_t *) v1;
        p2 = (uintptr_t *) v2;
        if (p1 < p2)
                return -1;
        if (p1 > p2)
                return 1;
        return 0;
}

static no_ignore word_t
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
        return ERR_ABSENT;
}

no_ignore word_t
xl_dagc_init(struct xl_dagc *graph)
{
        struct xl_dagc_node *p, *c1, *c2;
        struct __xl_dagc_adjacency *adj;
        size_t i, j, a;
        word_t err;

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
                                return ERR_UNEXPECTED_FAILURE;
                        graph->adjacency[a].n_parents++;
                }
                if (c2 != NULL)
                {
                        err = __find_adjacency(
                                &a, graph->adjacency, graph->n, c2);
                        if (err != OK)
                                return ERR_UNEXPECTED_FAILURE;
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
                                return ERR_UNEXPECTED_FAILURE;
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
                                return ERR_UNEXPECTED_FAILURE;
                        adj = &graph->adjacency[a];
                        for (j = 0; adj->parents[j] && j < adj->n_parents; j++);
                        adj->parents[j] = p;
                }
        }

        return OK;
}

no_ignore word_t
xl_dagc_known_value(
        struct xl_value **value,
        struct xl_value **type,
        struct xl_dagc_node *node)
{
        *type = node->known_type;

        switch (node->node_type)
        {
        case DAGC_NODE_APPLY:
                *value = ((struct xl_dagc_apply *) node)->known_value;
                return OK;
        case DAGC_NODE_LOAD:
                *value = ((struct xl_dagc_load *) node)->known_value;
                return OK;
        case DAGC_NODE_CONST:
                *value = ((struct xl_dagc_const *) node)->value;
                return OK;
        }
        return ERR_BAD_TYPE;
}

no_ignore word_t
xl_dagc_get_parents(
        struct xl_dagc_node ***parents,
        size_t *n_parents,
        struct xl_dagc *graph,
        struct xl_dagc_node *child)
{
        size_t i;
        word_t err;

        err = __find_adjacency(&i, graph->adjacency, graph->n, child);
        if (err != OK)
                return err;

        *parents = graph->adjacency[i].parents;
        *n_parents = graph->adjacency[i].n_parents;
        return OK;
}
