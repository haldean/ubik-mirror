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
#include <string.h>

#include "expel/assert.h"
#include "expel/dagc.h"
#include "expel/expel.h"

// DO NOT SUBMIT
#include "expel/util.h"
#include <stdio.h>

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
        case DAGC_NODE_INPUT:
        case DAGC_NODE_NATIVE:
                *d1 = NULL;
                *d2 = NULL;
                return OK;
        }
        return xl_raise(ERR_UNKNOWN_TYPE, "get deps");
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

        graph->tag = TAG_GRAPH;
        graph->refcount = 1;

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
                        a = graph->n;
                        err = __find_adjacency(
                                &a, graph->adjacency, graph->n, c1);
                        if (err != OK)
                                return err;
                        if (unlikely(a >= graph->n))
                                return xl_raise(ERR_UNEXPECTED_FAILURE, NULL);
                        graph->adjacency[a].n_parents++;
                }
                if (c2 != NULL)
                {
                        a = graph->n;
                        err = __find_adjacency(
                                &a, graph->adjacency, graph->n, c2);
                        if (unlikely(a >= graph->n))
                                return xl_raise(ERR_UNEXPECTED_FAILURE, NULL);
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
                        a = graph->n;
                        err = __find_adjacency(
                                &a, graph->adjacency, graph->n, c1);
                        if (err != OK)
                                return err;
                        if (unlikely(a >= graph->n))
                                return xl_raise(ERR_UNEXPECTED_FAILURE, NULL);
                        adj = &graph->adjacency[a];
                        /* Find the first NULL parent entry. */
                        for (j = 0; adj->parents[j] && j < adj->n_parents; j++);
                        adj->parents[j] = p;
                }
                if (c2 != NULL)
                {
                        a = graph->n;
                        err = __find_adjacency(
                                &a, graph->adjacency, graph->n, c2);
                        if (err != OK)
                                return err;
                        if (unlikely(a >= graph->n))
                                return xl_raise(ERR_UNEXPECTED_FAILURE, NULL);
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
        for (i = 0, next_out = 0; i < graph->n; i++)
        {
                p = graph->nodes[i];
                if (p->node_type == DAGC_NODE_INPUT)
                {
                        next_in = ((struct xl_dagc_input *) p)->arg_num;
                        xl_assert(next_in < graph->in_arity);
                        if (graph->inputs[next_in] != NULL)
                                return xl_raise(
                                        ERR_BAD_GRAPH,
                                        "multiple inputs with same arg num");
                        graph->inputs[next_in] = p;
                }
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
        *type = node->known_type;

        switch (node->node_type)
        {
        case DAGC_NODE_CONST:
        case DAGC_NODE_APPLY:
        case DAGC_NODE_LOAD:
        case DAGC_NODE_INPUT:
                if (node->value_type == DAGC_TYPE_VALUE)
                {
                        *value = node->known.tree;
                        return OK;
                }
                return xl_raise(ERR_BAD_TYPE, "known_value value type");
        }
        return xl_raise(ERR_BAD_TYPE, "known_value node type");
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

        i = graph->n;
        err = __find_adjacency(&i, graph->adjacency, graph->n, child);
        if (err != OK)
                return err;
        if (unlikely(i >= graph->n))
                return xl_raise(ERR_UNEXPECTED_FAILURE, NULL);

        *parents = graph->adjacency[i].parents;
        *n_parents = graph->adjacency[i].n_parents;
        return OK;
}

no_ignore static xl_error_t
__sizeof(size_t *res, struct xl_dagc_node *n)
{
        switch (n->node_type)
        {
        case DAGC_NODE_APPLY:
                *res = sizeof(struct xl_dagc_apply);
                return OK;
        case DAGC_NODE_CONST:
                *res = sizeof(struct xl_dagc_const);
                return OK;
        case DAGC_NODE_INPUT:
                *res = sizeof(struct xl_dagc_input);
                return OK;
        case DAGC_NODE_LOAD:
                *res = sizeof(struct xl_dagc_load);
                return OK;
        case DAGC_NODE_STORE:
                *res = sizeof(struct xl_dagc_store);
                return OK;
        case DAGC_NODE_NATIVE:
                *res = sizeof(struct xl_dagc_native_out);
                return OK;
        }
        return xl_raise(ERR_BAD_TYPE, "dagc sizeof");
}

no_ignore static xl_error_t
__replace_ref(
        struct xl_dagc_node **ref,
        struct xl_dagc_node **proto,
        struct xl_dagc_node **copied,
        size_t n)
{
        size_t i;

        if (*ref == NULL)
                return OK;
        for (i = 0; i < n; i++)
        {
                if (proto[i] == *ref)
                {
                        *ref = copied[i];
                        return OK;
                }
        }
        return xl_raise(ERR_ABSENT, "replace ref");
}

no_ignore static xl_error_t
__replace_node_refs(
        struct xl_dagc_node *node,
        struct xl_dagc_node **proto,
        struct xl_dagc_node **copied,
        size_t n)
{
        struct xl_dagc_apply *a;
        struct xl_dagc_load *l;
        struct xl_dagc_store *s;
        xl_error_t err;

        switch (node->node_type)
        {
        case DAGC_NODE_APPLY:
                a = (struct xl_dagc_apply *) node;
                err = __replace_ref(&a->func, proto, copied, n);
                if (err != OK)
                        return err;
                err = __replace_ref(&a->arg, proto, copied, n);
                return err;

        case DAGC_NODE_LOAD:
                l = (struct xl_dagc_load *) node;
                err = __replace_ref(&l->dependent_store, proto, copied, n);
                return err;

        case DAGC_NODE_STORE:
                s = (struct xl_dagc_store *) node;
                err = __replace_ref(&s->value, proto, copied, n);
                return err;

        case DAGC_NODE_CONST:
        case DAGC_NODE_INPUT:
        case DAGC_NODE_NATIVE:
                return OK;
        }
        return xl_raise(ERR_UNKNOWN_TYPE, "replace node refs");
}

no_ignore static xl_error_t
__increment_value_refs(struct xl_dagc_node *node)
{
        struct xl_dagc_load *l;
        struct xl_dagc_store *s;
        struct xl_dagc_const *c;
        struct xl_dagc_input *i;
        xl_error_t err;

        if (node->known.any != NULL)
        {
                err = xl_take(node->known.any);
                if (err != OK)
                        return err;
        }
        if (node->known_type != NULL)
        {
                err = xl_take(node->known_type);
                if (err != OK)
                        return err;
        }

        switch (node->node_type)
        {
        case DAGC_NODE_APPLY:
        case DAGC_NODE_NATIVE:
                return OK;

        case DAGC_NODE_INPUT:
                i = (struct xl_dagc_input *) node;
                return xl_take(i->required_type);

        case DAGC_NODE_LOAD:
                l = (struct xl_dagc_load *) node;
                return xl_take(l->loc);

        case DAGC_NODE_STORE:
                s = (struct xl_dagc_store *) node;
                return xl_take(s->loc);

        case DAGC_NODE_CONST:
                c = (struct xl_dagc_const *) node;
                err = xl_take(c->type);
                if (err != OK)
                        return err;
                return xl_take(c->value.any);
        }
        return xl_raise(ERR_UNKNOWN_TYPE, "inc value refs");
}

no_ignore xl_error_t
xl_dagc_copy(
        struct xl_dagc **res_ptr,
        struct xl_dagc *proto)
{
        struct __xl_dagc_adjacency *adj;
        struct xl_dagc *result;
        size_t i, j, size;
        xl_error_t err;

        /* Start by making a direct copy, then replace all of the references.
         * Since the nodes are all different sizes, this is unfortunately more
         * complicated than just a memcpy from proto to result :( */
        size = sizeof(struct xl_dagc);
        if (proto->tag & TAG_NATIVE_GRAPH)
                size = sizeof(struct xl_dagc_native);

        result = calloc(1, size);
        if (result == NULL)
                return xl_raise(ERR_NO_MEMORY, "dagc copy");
        *res_ptr = result;

        memcpy(result, proto, size);
        result->refcount = 0;

        result->nodes = calloc(proto->n, sizeof(struct xl_dagc_node *));
        for (i = 0; i < proto->n; i++)
        {
                err = __sizeof(&size, proto->nodes[i]);
                if (err != OK)
                        return err;
                result->nodes[i] = calloc(1, size);
                if (result->nodes[i] == NULL)
                        return xl_raise(ERR_NO_MEMORY, "dagc copy");
                memcpy(result->nodes[i], proto->nodes[i], size);
        }

        for (i = 0; i < result->n; i++)
        {
                err = __replace_node_refs(
                        result->nodes[i], proto->nodes, result->nodes,
                        result->n);
                if (err != OK)
                        return err;
                err = __increment_value_refs(result->nodes[i]);
                if (err != OK)
                        return err;
        }

        result->inputs =
                calloc(result->in_arity, sizeof(struct xl_dagc_node *));
        result->terminals =
                calloc(result->out_arity, sizeof(struct xl_dagc_node *));
        for (i = 0; i < result->in_arity; i++)
        {
                result->inputs[i] = proto->inputs[i];
                err = __replace_ref(
                        &result->inputs[i], proto->nodes, result->nodes,
                        result->n);
                if (err != OK)
                        return err;
        }
        for (i = 0; i < result->out_arity; i++)
        {
                result->terminals[i] = proto->terminals[i];
                err = __replace_ref(
                        &result->terminals[i], proto->nodes, result->nodes,
                        result->n);
                if (err != OK)
                        return err;
        }

        result->adjacency =
                calloc(result->n, sizeof(struct __xl_dagc_adjacency));
        memcpy(result->adjacency, proto->adjacency,
               result->n * sizeof(struct __xl_dagc_adjacency));

        for (i = 0; i < result->n; i++)
        {
                adj = &result->adjacency[i];
                err = __replace_ref(
                        &adj->child, proto->nodes, result->nodes, result->n);
                if (err != OK)
                        return err;

                adj->parents =
                        calloc(adj->n_parents, sizeof(struct xl_dagc_node *));
                memcpy(adj->parents, &proto->adjacency[i], adj->n_parents);
                for (j = 0; j < adj->n_parents; j++)
                {
                        err = __replace_ref(
                                &adj->parents[j], proto->nodes,
                                result->nodes, result->n);
                        if (err != OK)
                                return err;
                }
        }

        return OK;
}
