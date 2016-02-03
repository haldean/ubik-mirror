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

#include "expel/apply.h"
#include "expel/assert.h"
#include "expel/dagc.h"
#include "expel/env.h"
#include "expel/expel.h"
#include "expel/explain.h"
#include "expel/util.h"

struct xl_node_schedule
{
        struct xl_node_schedule *prev;

        struct xl_dagc *graph;
        struct xl_env *env;
        struct xl_dagc_node *node;

        struct xl_node_schedule *bind_result;
};

struct xl_scheduler
{
        struct xl_node_schedule *ready;
};

/* Initializes the flags of a given set of nodes.
 *
 * A node is ready if it has no incomplete dependencies, and is
 * complete if it has no dependencies. */
no_ignore static xl_error
_set_initial_ready(struct xl_dagc_node **nodes, size_t n_nodes)
{
        struct xl_dagc_node *n, *d1, *d2, *d3;
        xl_error err;
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

                /* Cond nodes are also special; they start out only waiting on
                 * their condition, and then on the basis of their condition
                 * they are re-waited. */
                if (n->node_type == DAGC_NODE_COND)
                {
                        if (d1->flags & XL_DAGC_FLAG_COMPLETE)
                                n->flags = 0;
                        else
                                n->flags = XL_DAGC_FLAG_WAIT_D1;
                }
                else
                {
                        n->flags = XL_DAGC_WAIT_MASK & ~XL_DAGC_FLAG_WAIT_EVAL;
                        if (d1 == NULL || d1->flags & XL_DAGC_FLAG_COMPLETE)
                                n->flags ^= XL_DAGC_FLAG_WAIT_D1;
                        if (d2 == NULL || d2->flags & XL_DAGC_FLAG_COMPLETE)
                                n->flags ^= XL_DAGC_FLAG_WAIT_D2;
                        if (d3 == NULL || d3->flags & XL_DAGC_FLAG_COMPLETE)
                                n->flags ^= XL_DAGC_FLAG_WAIT_D3;
                }
        }

        return OK;
}

no_ignore static xl_error
_schedule(
        struct xl_scheduler *schedule,
        struct xl_node_schedule *copy_from)
{
        struct xl_node_schedule *sched;
        xl_error err;

        xl_assert(!(copy_from->node->flags & XL_DAGC_WAIT_MASK));

        sched = calloc(1, sizeof(struct xl_node_schedule));
        *sched = *copy_from;
        sched->prev = schedule->ready;
        schedule->ready = sched;

        err = xl_take(sched->graph);
        if (err != OK)
                return err;

        return OK;
}

no_ignore static xl_error
_notify_parents(struct xl_scheduler *s, struct xl_node_schedule *n)
{
        struct xl_dagc_node **parents, *d1, *d2, *d3, *p;
        struct xl_node_schedule ps;
        size_t i, n_parents;
        xl_error err;

        err = xl_dagc_get_parents(&parents, &n_parents, n->graph, n->node);
        if (err != OK)
                return err;

        for (i = 0; i < n_parents; i++)
        {
                p = parents[i];

                err = xl_dagc_get_deps(&d1, &d2, &d3, p);
                if (err != OK)
                        return err;

                if (n->node == d1)
                        p->flags &= ~XL_DAGC_FLAG_WAIT_D1;
                if (n->node == d2)
                        p->flags &= ~XL_DAGC_FLAG_WAIT_D2;
                if (n->node == d3)
                        p->flags &= ~XL_DAGC_FLAG_WAIT_D3;

                if (!(p->flags & XL_DAGC_WAIT_MASK))
                {
                        #ifdef XL_SCHEDULE_DEBUG
                        fprintf(stderr, "scheduling %s for %s\n",
                                xl_explain_node(p), xl_explain_node(n->node));
                        #endif
                        ps.node = p;
                        ps.graph = n->graph;
                        ps.bind_result = NULL;
                        err = xl_schedule_push(s, &ps);
                        if (err != OK)
                                return err;
                }
        }

        if (n->bind_result != NULL)
        {
                err = xl_release(n->bind_result->node->known.any);
                if (err != OK)
                        return err;

                n->bind_result->node->known = n->node->known;
                err = xl_take(n->bind_result->node->known.any);
                if (err != OK)
                        return err;

                n->bind_result->node->known_type = n->node->known_type;
                err = xl_take(n->bind_result->node->known_type);
                if (err != OK)
                        return err;

                n->bind_result->node->flags = XL_DAGC_FLAG_COMPLETE;
        }
        return OK;
}

no_ignore xl_error
xl_schedule_push(struct xl_scheduler *s, struct xl_node_schedule *n)
{
        struct xl_dagc_node *d1, *d2, *d3;
        struct xl_node_schedule ds;
        xl_error err;

        if (n->node->flags & XL_DAGC_FLAG_COMPLETE)
        {
                #ifdef XL_SCHEDULE_DEBUG
                fprintf(stderr, "%s complete\n", xl_explain_node(n->node));
                #endif
                return _notify_parents(s, n);
        }
        if (n->node->flags & XL_DAGC_WAIT_MASK)
        {
                #ifdef XL_SCHEDULE_DEBUG
                fprintf(stderr, "%s waiting, queueing children\n",
                        xl_explain_node(n->node));
                #endif

                err = xl_dagc_get_deps(&d1, &d2, &d3, n->node);
                if (err != OK)
                        return err;

                ds.graph = n->graph;
                ds.env = n->env;
                ds.bind_result = NULL;

                if (n->node->flags & XL_DAGC_FLAG_WAIT_D1)
                {
                        xl_assert(d1 != NULL);
                        ds.node = d1;
                        err = xl_schedule_push(s, &ds);
                        if (err != OK)
                                return err;
                }
                if (n->node->flags & XL_DAGC_FLAG_WAIT_D2)
                {
                        xl_assert(d2 != NULL);
                        ds.node = d2;
                        err = xl_schedule_push(s, &ds);
                        if (err != OK)
                                return err;
                }
                if (n->node->flags & XL_DAGC_FLAG_WAIT_D3)
                {
                        xl_assert(d3 != NULL);
                        ds.node = d3;
                        err = xl_schedule_push(s, &ds);
                        if (err != OK)
                                return err;
                }

                return OK;
        }

        #ifdef XL_SCHEDULE_DEBUG
        fprintf(stderr, "%s ready, queueing\n",
                xl_explain_node(n->node));
        #endif
        return _schedule(s, n);
}

no_ignore static xl_error
_eval_native_dagc(struct xl_env *env, struct xl_dagc_native *ngraph)
{
        xl_error err;
        struct xl_dagc *graph;

        graph = (struct xl_dagc *) ngraph;
        err = ngraph->evaluator(env, graph);
        if (err != OK)
                return err;

        graph->result->flags |= XL_DAGC_FLAG_COMPLETE;
        return OK;
}

no_ignore xl_error
xl_dagc_schedule(
        struct xl_scheduler *s,
        struct xl_dagc *graph,
        struct xl_env *env,
        struct xl_node_schedule *bind_result)
{
        size_t i;
        struct xl_node_schedule ts;
        xl_error err;

        err = _set_initial_ready(graph->nodes, graph->n);
        if (err != OK)
                return err;

        for (i = 0; i < graph->out_arity; i++)
        {
                ts.node = graph->terminals[i];
                ts.graph = graph;
                ts.env = env;
                ts.bind_result = NULL;
                if (ts.node == graph->result && bind_result != NULL)
                {
                        ts.bind_result = calloc(
                                1, sizeof(struct xl_node_schedule));
                        *ts.bind_result = *bind_result;
                }
                err = xl_schedule_push(s, &ts);
                if (err != OK)
                        return err;
        }

        return OK;
}

no_ignore xl_error
xl_dagc_collapse_graph(
        struct xl_scheduler *s,
        struct xl_node_schedule *ns,
        struct xl_env *env)
{
        struct xl_dagc *graph;
        struct xl_env *child_env;
        xl_error err;

        if ((*ns->node->known.tag & TAG_TYPE_MASK) != TAG_GRAPH)
                return OK;
        graph = ns->node->known.graph;

        if (graph->in_arity != 0)
                return OK;
        /* Graph is fully applied; we can evaluate it to find the value of this
         * node. */

        /* Native graphs get to cheat and skip all this biz. */
        if (graph->tag & TAG_GRAPH_NATIVE)
        {
                err = _eval_native_dagc(env, (struct xl_dagc_native *) graph);
                if (err != OK)
                        return err;

                ns->node->known.any = graph->result->known.any;
                err = xl_take(ns->node->known.any);
                if (err != OK)
                        return err;
                ns->node->known_type = graph->result->known_type;
                err = xl_take(ns->node->known_type);
                if (err != OK)
                        return err;

                return OK;
        }

        /* Create a child environment to execute the function in. */
        child_env = calloc(1, sizeof(struct xl_env));
        err = xl_env_make_child(child_env, env);
        if (err != OK)
                return err;

        ns->node->flags |= XL_DAGC_FLAG_WAIT_EVAL;
        ns->node->flags &= ~XL_DAGC_FLAG_COMPLETE;

        /* TODO: free child env when finished */
        err = xl_dagc_schedule(s, graph, child_env, ns);
        if (err != OK)
                return err;

        err = xl_release(graph);
        return err;
}

no_ignore xl_error
xl_dagc_eval(struct xl_env *env, struct xl_dagc *graph)
{
        struct xl_scheduler s;
        struct xl_node_schedule *to_exec;
        xl_error err;

        /* Native graphs get to cheat and skip all this biz. */
        if (graph->tag & TAG_GRAPH_NATIVE)
                return _eval_native_dagc(env, (struct xl_dagc_native *) graph);

        s.ready = NULL;

        err = xl_dagc_schedule(&s, graph, env, NULL);
        if (err != OK)
                return err;

        while (s.ready != NULL)
        {
                to_exec = s.ready;
                s.ready = to_exec->prev;

                if (to_exec->node->flags & XL_DAGC_FLAG_COMPLETE)
                {
                        free(to_exec);
                        continue;
                }

                err = xl_dagc_node_eval(env, to_exec->node);
                if (err != OK)
                        return err;

                if (to_exec->node->flags & XL_DAGC_FLAG_COMPLETE)
                {
                        err = xl_dagc_collapse_graph(&s, to_exec, env);
                        if (err != OK)
                                return err;
                }

                err = xl_schedule_push(&s, to_exec);
                if (err != OK)
                        return err;

                free(to_exec);
        }
        return OK;
}
