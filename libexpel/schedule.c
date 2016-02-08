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
#include <stdlib.h>

#include "expel/dagc.h"
#include "expel/env.h"
#include "expel/explain.h"
#include "expel/schedule.h"
#include "expel/util.h"

struct xl_scheduler
{
        struct xl_exec_unit *wait;
        struct xl_exec_unit *ready;
};

/* Creates a scheduler. */
no_ignore xl_error
xl_schedule_new(struct xl_scheduler **s)
{
        *s = calloc(1, sizeof(struct xl_scheduler));
        if (*s == NULL)
                return xl_raise(ERR_NO_MEMORY, "schedule alloc");
        return OK;
}

/* Destroys a scheduler. */
no_ignore xl_error
xl_schedule_free(struct xl_scheduler *s)
{
        struct xl_exec_unit *to_free;

        while (s->wait != NULL)
        {
                to_free = s->wait;
                s->wait = s->wait->next;
                free(to_free);
        }

        while (s->ready != NULL)
        {
                to_free = s->ready;
                s->ready = s->ready->next;
                free(to_free);
        }

        return OK;
}

/* Initializes the flags of a given node. */
no_ignore static xl_error
_set_initial_ready(struct xl_dagc_node *n)
{
        struct xl_dagc_node *d1, *d2, *d3;
        xl_error err;

        /* Input nodes are special; they're only ready once their values
         * have been filled in, even though they have no dependencies.
         * Only application of the graph they participate in can make
         * them ready, so these don't changed here. */
        if (n->node_type == DAGC_NODE_INPUT)
                return OK;

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
                n->flags = 0;
                if (d1 != NULL && !(d1->flags & XL_DAGC_FLAG_COMPLETE))
                        n->flags |= XL_DAGC_FLAG_WAIT_D1;
                if (d2 != NULL && !(d2->flags & XL_DAGC_FLAG_COMPLETE))
                        n->flags |= XL_DAGC_FLAG_WAIT_D2;
                if (d3 != NULL && !(d3->flags & XL_DAGC_FLAG_COMPLETE))
                        n->flags |= XL_DAGC_FLAG_WAIT_D3;
        }

        return OK;
}

no_ignore static xl_error
_enqueue(
        struct xl_scheduler *s,
        struct xl_dagc *graph,
        struct xl_env *env,
        struct xl_exec_notify *notify,
        struct xl_dagc_node *node)
{
        struct xl_exec_unit *u;
        xl_error err;

        u = calloc(1, sizeof(struct xl_exec_unit));
        if (u == NULL)
                return xl_raise(ERR_NO_MEMORY, "exec unit alloc");
        u->node = node;
        u->notify = notify;
        u->env = env;
        u->graph = graph;

        err = _set_initial_ready(node);
        if (err != OK)
                return err;

        u->next = s->wait;
        s->wait = u;

        err = xl_take(graph);
        if (err != OK)
                return err;

        return OK;
}

no_ignore static xl_error
_eval_native_dagc(
        struct xl_dagc_native *ngraph,
        struct xl_env *env,
        struct xl_exec_notify *notify)
{
        xl_error err;
        struct xl_dagc *graph;
        struct xl_exec_unit unit;

        graph = (struct xl_dagc *) ngraph;
        err = ngraph->evaluator(env, graph);
        if (err != OK)
                return err;

        graph->result->flags |= XL_DAGC_FLAG_COMPLETE;

        if (notify == NULL)
                return OK;

        unit.node = graph->result;
        unit.graph = graph;
        unit.env = env;
        unit.notify = NULL;
        unit.next = NULL;

        err = notify->notify(notify->arg, &unit);
        if (err != OK)
                return err;

        return OK;
}

/* Pushes a graph into the scheduler for execution. */
no_ignore xl_error
xl_schedule_push(
        struct xl_scheduler *s,
        struct xl_dagc *graph,
        struct xl_env *env,
        struct xl_exec_notify *notify)
{
        xl_error err;
        size_t i;

        /* Native graphs get to cheat and skip all this biz. */
        if (graph->tag & TAG_GRAPH_NATIVE)
                return _eval_native_dagc(
                        (struct xl_dagc_native *) graph, env, notify);

        for (i = 0; i < graph->n; i++)
        {
                err = _set_initial_ready(graph->nodes[i]);
                if (err != OK)
                        return err;
        }

        /* TODO: only exec reachable nodes */
        for (i = 0; i < graph->n; i++)
        {
                err = _enqueue(s, graph, env, notify, graph->nodes[i]);
                if (err != OK)
                        return err;
        }

        return OK;
}

/* Marks an execution unit complete. */
no_ignore xl_error
xl_schedule_complete(
        struct xl_scheduler *s,
        struct xl_exec_unit *e)
{
        struct xl_dagc_node **parents;
        struct xl_dagc_node *d1, *d2, *d3;
        size_t n_parents;
        size_t i;
        xl_error err;

        unused(s);

        err = xl_dagc_get_parents(&parents, &n_parents, e->graph, e->node);
        if (err != OK)
                return err;

        for (i = 0; i < n_parents; i++)
        {
                err = xl_dagc_get_deps(&d1, &d2, &d3, parents[i]);
                if (err != OK)
                        return err;

                if (d1 == e->node)
                        parents[i]->flags &= ~XL_DAGC_FLAG_WAIT_D1;
                if (d2 == e->node)
                        parents[i]->flags &= ~XL_DAGC_FLAG_WAIT_D2;
                if (d3 == e->node)
                        parents[i]->flags &= ~XL_DAGC_FLAG_WAIT_D3;
        }

        free(e);
        return OK;
}

no_ignore static xl_error
_collapse_graph(
        struct xl_scheduler *s,
        struct xl_exec_unit *e)
{
        unused(s);
        unused(e);

        return xl_raise(ERR_NOT_IMPLEMENTED, "collapse_graph");
}

no_ignore static bool
_is_ready(struct xl_exec_unit *e)
{
        return !(e->node->flags & XL_DAGC_WAIT_MASK);
}

/* Runs a single pass of the scheduler. */
no_ignore static xl_error
_run_single_pass(struct xl_scheduler *s)
{
        struct xl_exec_unit *u, *t;
        struct xl_exec_unit *prev;
        xl_error err;

        /* This proceeds in two phases; first, we move everything that is ready
         * to be executed from the wait pile to the ready pile, then we execute
         * everything in the ready pile. */
        u = s->wait;
        prev = NULL;
        while (u != NULL)
        {
                t = u->next;

                if (_is_ready(u))
                {
                        u->next = s->ready;
                        s->ready = u;
                        if (prev != NULL)
                                prev->next = t;
                        else
                                s->wait = t;
                }
                else
                {
                        prev = u;
                }

                u = t;
        }

        /* If the ready pile is still empty, then we're deadlocked. */
        if (s->ready == NULL)
                return xl_raise(ERR_DEADLOCK, "all jobs are waiting");

        /* Now all of the ready jobs are in the ready pile, so we just have to
         * execute them. */
        u = s->ready;
        while (u != NULL)
        {
                err = xl_dagc_node_eval(u->env, u->node);
                if (err != OK)
                        return err;

                s->ready = s->ready->next;

                if (*u->node->known.tag & TAG_GRAPH &&
                        u->node->known.graph->in_arity == 0)
                {
                        /* Here, we collapse the graph and don't mark the things
                         * depending on the node as ready; when we finish
                         * collapsing the graph we'll notify the dependent
                         * nodes. */
                        err = _collapse_graph(s, u);
                        if (err != OK)
                                return err;
                }
                else if (u->node->flags & XL_DAGC_FLAG_COMPLETE)
                {
                        err = xl_schedule_complete(s, u);
                        if (err != OK)
                                return err;
                }
                else if (u->node->flags & XL_DAGC_WAIT_MASK)
                {
                        u->next = s->wait;
                        s->wait = u;
                }
                else return xl_raise(
                        ERR_BAD_HEADER,
                        "eval'ed node is not complete or waiting");

                u = s->ready;
        }

        return OK;
}

/* Runs all queued jobs on the scheduler. */
no_ignore xl_error
xl_schedule_run(struct xl_scheduler *s)
{
        xl_error err;

        while (s->wait != NULL || s->ready != NULL)
        {
                err = _run_single_pass(s);
                if (err != OK)
                        return err;
        }
        return OK;
}
