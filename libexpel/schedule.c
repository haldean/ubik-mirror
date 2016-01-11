/*
 * schedule.c: scheduled evaluation of directed acyclic graphs of computation
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
#include <stdio.h>

#include "expel/dagc.h"
#include "expel/env.h"
#include "expel/expel.h"
#include "expel/util.h"

struct xl_node_schedule
{
        struct xl_node_schedule *prev;
        struct xl_dagc_node *node;
};

/* Adds elem to set if elem isn't already in set, modifying the
 * provided size if necessary. Returns true if the element was
 * added to the set. */
static bool
__set_add(struct xl_dagc_node **set, size_t *n, struct xl_dagc_node *elem)
{
        size_t i;
        bool found;

        found = false;
        for (i = 0; i < *n; i++)
        {
                if (set[i] == elem)
                {
                        found = true;
                        break;
                }
        }
        if (!found)
        {
                set[(*n)++] = elem;
                return true;
        }
        return false;
}

/* Finds reachable nodes in the graph.
 *
 * "Reachable" nodes are ones that are reachable from a terminal
 * node by traversing dependency edges; nodes that are not
 * reachable do not need to be evaluated. */
no_ignore static xl_error_t
__find_reachable_nodes(
        struct xl_dagc_node **reachable,
        size_t *rn,
        struct xl_dagc *graph)
{
        struct xl_dagc_node *n, *d1, *d2, *d3;
        size_t i;
        xl_error_t err;
        bool done;

        /* Mark appropriate nodes as reachable. */
        *rn = 0;

        /* First find terminal nodes. */
        for (i = 0; i < graph->n; i++)
        {
                n = graph->nodes[i];
                if (n->is_terminal)
                        reachable[(*rn)++] = n;
        }

        /* Now keep finding nodes reachable from reachable nodes
         * until there are no more. This exploits the moderately
         * surprising behavior that our loop maximum keeps going
         * up as we add things to the reachable set, so they will
         * be reached later in the loop. Kind of a neat trick! */
        for (i = 0; i < *rn; i++)
        {
                n = reachable[i];
                #ifdef XL_SCHEDULE_DEBUG
                fprintf(stderr, "reachable  %hx\n", (short)((uintptr_t) n));
                #endif
                err = xl_dagc_get_deps(&d1, &d2, &d3, n);
                if (err != OK)
                        return err;
                if (d1 != NULL)
                        done &= !__set_add(reachable, rn, d1);
                if (d2 != NULL)
                        done &= !__set_add(reachable, rn, d2);
                if (d3 != NULL)
                        done &= !__set_add(reachable, rn, d3);
        }

        return OK;
}

/* Initializes the flags of a given set of nodes.
 *
 * A node is ready if it has no incomplete dependencies, and is
 * complete if it has no dependencies. */
no_ignore static xl_error_t
__set_initial_ready(struct xl_dagc_node **nodes, size_t n_nodes)
{
        struct xl_dagc_node *n, *d1, *d2, *d3;
        xl_error_t err;
        size_t i;

        /* Set the ready flag on everyone whose dependencies are
         * complete. */
        for (i = 0; i < n_nodes; i++)
        {
                n = nodes[i];
                /* Input nodes are special; they're only ready once their values
                 * have been filled in, even though they have no dependencies.
                 * Only application of the graph they participate in can make
                 * them ready, so these don't changed here. */
                if (n->node_type == DAGC_NODE_INPUT)
                        continue;

                err = xl_dagc_get_deps(&d1, &d2, &d3, n);
                if (err != OK)
                        return err;

                if (d1 == NULL || d1->flags & XL_DAGC_FLAG_COMPLETE)
                        n->flags |= XL_DAGC_FLAG_D1_READY;
                if (d2 == NULL || d2->flags & XL_DAGC_FLAG_COMPLETE)
                        n->flags |= XL_DAGC_FLAG_D2_READY;
                if (d3 == NULL || d3->flags & XL_DAGC_FLAG_COMPLETE)
                        n->flags |= XL_DAGC_FLAG_D3_READY;
        }

        return OK;
}

no_ignore static xl_error_t
__schedule(struct xl_node_schedule **schedule, struct xl_dagc_node *node)
{
        struct xl_node_schedule *sched;

        sched = calloc(1, sizeof(struct xl_node_schedule));
        sched->prev = *schedule;
        sched->node = node;
        *schedule = sched;

        return OK;
}

no_ignore static xl_error_t
__schedule_all_ready(
                struct xl_node_schedule **schedule,
                struct xl_dagc_node **nodes,
                size_t n_nodes)
{
        struct xl_dagc_node *n;
        xl_error_t err;
        size_t i;

        *schedule = NULL;
        for (i = 0; i < n_nodes; i++)
        {
                n = nodes[i];
                if (n->flags & XL_DAGC_FLAG_COMPLETE)
                        continue;
                if ((n->flags & XL_DAGC_READY_MASK) == XL_DAGC_READY_MASK)
                {
                        err = __schedule(schedule, n);
                        if (err != OK)
                                return err;
                }
        }

        return OK;
}

no_ignore static xl_error_t
__notify_parents(
                struct xl_node_schedule **schedule,
                struct xl_dagc *graph,
                struct xl_dagc_node *node)
{
        struct xl_dagc_node **parents, *d1, *d2, *d3, *p;
        size_t i, n_parents;
        xl_error_t err;

        err = xl_dagc_get_parents(&parents, &n_parents, graph, node);
        if (err != OK)
                return err;

        for (i = 0; i < n_parents; i++)
        {
                p = parents[i];

                err = xl_dagc_get_deps(&d1, &d2, &d3, p);
                if (err != OK)
                        return err;

                if (node == d1)
                        p->flags |= XL_DAGC_FLAG_D1_READY;
                if (node == d2)
                        p->flags |= XL_DAGC_FLAG_D2_READY;
                if (node == d3)
                        p->flags |= XL_DAGC_FLAG_D3_READY;

                if ((p->flags & XL_DAGC_READY_MASK) == XL_DAGC_READY_MASK)
                {
                        #ifdef XL_SCHEDULE_DEBUG
                        fprintf(stderr, "scheduling %hx for %hx\n",
                                (short)((uintptr_t) p),
                                (short)((uintptr_t) node));
                        #endif
                        err = __schedule(schedule, p);
                        if (err != OK)
                                return err;
                }
        }
        return OK;
}

no_ignore static xl_error_t
__eval_native_dagc(struct xl_env *env, struct xl_dagc_native *ngraph)
{
        xl_error_t err;
        struct xl_dagc *graph;

        graph = (struct xl_dagc *) ngraph;
        err = ngraph->evaluator(env, graph);
        if (err != OK)
                return err;

        graph->result->flags |= XL_DAGC_FLAG_COMPLETE;
        return OK;
}

no_ignore xl_error_t
xl_dagc_eval(struct xl_env *env, struct xl_dagc *graph)
{
        struct xl_dagc_node **reachable;
        struct xl_node_schedule *schedule, *to_exec;
        size_t n_nodes;
        xl_error_t err;

        /* Native graphs get to cheat and skip all this biz. */
        if (graph->tag & TAG_GRAPH_NATIVE)
                return __eval_native_dagc(env, (struct xl_dagc_native *) graph);

        reachable = calloc(graph->n, sizeof(struct xl_node *));
        if (reachable == NULL)
                return xl_raise(ERR_NO_MEMORY, "eval");

        err = __find_reachable_nodes(reachable, &n_nodes, graph);
        if (err != OK)
                return err;

        err = __set_initial_ready(reachable, n_nodes);
        if (err != OK)
                return err;

        err = __schedule_all_ready(&schedule, reachable, n_nodes);
        if (err != OK)
                return err;

        while (schedule != NULL)
        {
                to_exec = schedule;
                schedule = to_exec->prev;

                err = xl_dagc_node_eval(env, to_exec->node);
                if (err != OK)
                        return err;

                #ifdef XL_SCHEDULE_DEBUG
                        fprintf(stderr, "evaluated  %hx  %s\n",
                                (short)((uintptr_t) to_exec->node),
                                xl_explain_word(to_exec->node->node_type));
                #endif

                err = __notify_parents(&schedule, graph, to_exec->node);
                if (err != OK)
                        return err;

                free(to_exec);
        }

        free(reachable);
        return OK;
}
