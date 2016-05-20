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

#include "ubik/assert.h"
#include "ubik/dagc.h"
#include "ubik/env.h"
#include "ubik/schedule.h"
#include "ubik/util.h"

struct ubik_scheduler
{
        struct ubik_exec_unit *wait;
        struct ubik_exec_unit *ready;
};

/* Creates a scheduler. */
no_ignore ubik_error
ubik_schedule_new(struct ubik_scheduler **s)
{
        *s = calloc(1, sizeof(struct ubik_scheduler));
        if (*s == NULL)
                return ubik_raise(ERR_NO_MEMORY, "schedule alloc");
        return OK;
}

/* Destroys a scheduler. */
no_ignore ubik_error
ubik_schedule_free(struct ubik_scheduler *s)
{
        struct ubik_exec_unit *to_free;

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
no_ignore static ubik_error
_set_initial_ready(struct ubik_dagc_node *n)
{
        struct ubik_dagc_node *d1, *d2, *d3;
        ubik_error err;

        /* Input nodes are special; they're only ready once their values
         * have been filled in, even though they have no dependencies.
         * Only application of the graph they participate in can make
         * them ready, so these don't changed here. */
        if (n->node_type == DAGC_NODE_INPUT)
                return OK;

        err = ubik_dagc_get_deps(&d1, &d2, &d3, n);
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

no_ignore static ubik_error
_enqueue(
        struct ubik_scheduler *s,
        struct ubik_dagc *graph,
        struct ubik_env *env,
        struct ubik_exec_notify *notify,
        struct ubik_dagc_node *node)
{
        struct ubik_exec_unit *u;
        ubik_error err;
        struct ubik_exec_unit *test;

        test = s->wait;
        while (test != NULL)
        {
                if (test->node == node)
                        return OK;
                test = test->next;
        }

        test = s->ready;
        while (test != NULL)
        {
                if (test->node == node)
                        return OK;
                test = test->next;
        }

        u = calloc(1, sizeof(struct ubik_exec_unit));
        if (u == NULL)
                return ubik_raise(ERR_NO_MEMORY, "exec unit alloc");
        u->node = node;
        u->notify = notify;
        u->env = env;
        u->graph = graph;

        err = _set_initial_ready(node);
        if (err != OK)
                return err;

        u->next = s->wait;
        s->wait = u;

        err = ubik_take(graph);
        if (err != OK)
                return err;

        return OK;
}

no_ignore static ubik_error
_eval_native_dagc(
        struct ubik_scheduler *s,
        struct ubik_dagc_native *ngraph,
        struct ubik_env *env,
        struct ubik_exec_notify *notify)
{
        ubik_error err;
        struct ubik_dagc *graph;
        struct ubik_exec_unit unit;

        graph = (struct ubik_dagc *) ngraph;
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

        err = notify->notify(notify->arg, s, &unit);
        if (err != OK)
                return err;
        free(notify);

        return OK;
}

no_ignore static ubik_error
_push_dep_tree(
        struct ubik_scheduler *s,
        struct ubik_dagc *graph,
        struct ubik_dagc_node *node,
        struct ubik_env *env,
        struct ubik_exec_notify *notify);

/* Enqueues the nodes on which the provided node is waiting. */
no_ignore static ubik_error
_push_deps(
        struct ubik_scheduler *s,
        struct ubik_dagc *graph,
        struct ubik_dagc_node *node,
        struct ubik_env *env)
{
        struct ubik_dagc_node *d1, *d2, *d3;
        ubik_error err;

        err = ubik_dagc_get_deps(&d1, &d2, &d3, node);
        if (err != OK)
                return err;

        if (d1 != NULL && (node->flags & XL_DAGC_FLAG_WAIT_D1))
        {
                /* only the root gets notified; everything below it doesn't */
                err = _push_dep_tree(s, graph, d1, env, NULL);
                if (err != OK)
                        return err;
        }
        if (d2 != NULL && (node->flags & XL_DAGC_FLAG_WAIT_D2))
        {
                err = _push_dep_tree(s, graph, d2, env, NULL);
                if (err != OK)
                        return err;
        }
        if (d3 != NULL && (node->flags & XL_DAGC_FLAG_WAIT_D3))
        {
                err = _push_dep_tree(s, graph, d3, env, NULL);
                if (err != OK)
                        return err;
        }

        return OK;
}

/* Enqueues a node and all applicable dependencies. */
no_ignore static ubik_error
_push_dep_tree(
        struct ubik_scheduler *s,
        struct ubik_dagc *graph,
        struct ubik_dagc_node *node,
        struct ubik_env *env,
        struct ubik_exec_notify *notify)
{
        ubik_error err;

        err = _enqueue(s, graph, env, notify, node);
        if (err != OK)
                return err;

        err = _push_deps(s, graph, node, env);
        if (err != OK)
                return err;

        return OK;
}

/* Pushes a graph into the scheduler for execution. */
no_ignore ubik_error
ubik_schedule_push(
        struct ubik_scheduler *s,
        struct ubik_dagc *graph,
        struct ubik_env *env,
        struct ubik_exec_notify *notify)
{
        ubik_error err;
        struct ubik_dagc_node *n;
        size_t i;

        /* Native graphs get to cheat and skip all this biz. */
        if (graph->tag & TAG_GRAPH_NATIVE)
                return _eval_native_dagc(
                        s, (struct ubik_dagc_native *) graph, env, notify);

        for (i = 0; i < graph->n; i++)
        {
                err = _set_initial_ready(graph->nodes[i]);
                if (err != OK)
                        return err;
        }

        for (i = 0; i < graph->out_arity; i++)
        {
                n = graph->terminals[i];
                err = _push_dep_tree(
                        s, graph, n, env, n == graph->result ? notify : NULL);
                if (err != OK)
                        return err;
        }

        return OK;
}

/* Marks an execution unit complete. */
no_ignore ubik_error
ubik_schedule_complete(
        struct ubik_scheduler *s,
        struct ubik_exec_unit *e)
{
        struct ubik_dagc_node **parents;
        struct ubik_dagc_node *d1, *d2, *d3;
        size_t n_parents;
        size_t i;
        ubik_error err;

        err = ubik_dagc_get_parents(&parents, &n_parents, e->graph, e->node);
        if (err != OK)
                return err;

        for (i = 0; i < n_parents; i++)
        {
                err = ubik_dagc_get_deps(&d1, &d2, &d3, parents[i]);
                if (err != OK)
                        return err;

                if (d1 == e->node)
                        parents[i]->flags &= ~XL_DAGC_FLAG_WAIT_D1;
                if (d2 == e->node)
                        parents[i]->flags &= ~XL_DAGC_FLAG_WAIT_D2;
                if (d3 == e->node)
                        parents[i]->flags &= ~XL_DAGC_FLAG_WAIT_D3;
        }

        if (e->notify != NULL)
        {
                err = e->notify->notify(e->notify->arg, s, e);
                if (err != OK)
                        return err;
                free(e->notify);
        }

        err = ubik_release(e->graph);
        if (err != OK)
                return err;
        free(e);
        return OK;
}

ubik_error
_notify_node(
        struct ubik_exec_unit *waiting,
        struct ubik_scheduler *s,
        struct ubik_exec_unit *complete)
{
        void *old;
        void *old_type;
        ubik_error err;

#ifdef XL_SCHEDULE_DEBUG
        printf("notifying %s on completion of %s\n",
               ubik_node_explain(waiting->node), ubik_node_explain(complete->node));
#endif

        /* We save these off here because we need to free them, but freeing them
         * will free the result values as well. We read the result values into
         * our node and then free these when we're done instead. */
        old = waiting->node->known.any;
        old_type = waiting->node->known_type;

        waiting->node->known.any = complete->node->known.any;
        err = ubik_take(waiting->node->known.any);
        if (err != OK)
                return err;

        waiting->node->known_type = complete->node->known_type;
        err = ubik_take(waiting->node->known_type);
        if (err != OK)
                return err;

        err = ubik_env_free(complete->env);
        if (err != OK)
                return err;
        free(complete->env);

        err = ubik_release(old);
        if (err != OK)
                return err;

        err = ubik_release(old_type);
        if (err != OK)
                return err;

        err = ubik_release(complete->graph);
        if (err != OK)
                return err;

        waiting->node->flags = XL_DAGC_FLAG_COMPLETE;
        err = ubik_schedule_complete(s, waiting);
        if (err != OK)
                return err;

        return OK;
}

no_ignore static ubik_error
_collapse_graph(
        struct ubik_scheduler *s,
        struct ubik_exec_unit *e)
{
        struct ubik_exec_notify *notify;
        struct ubik_env *child_env;
        ubik_error err;

#if XL_SCHEDULE_DEBUG
        {
                char *buf = ubik_uri_explain(e->node->known.graph->identity);
                printf("collapsing node graph %s to value\n", buf);
                free(buf);
        }
#endif

        notify = calloc(1, sizeof(struct ubik_exec_notify));
        if (notify == NULL)
                return ubik_raise(ERR_NO_MEMORY, "exec notify");
        notify->notify = (ubik_exec_notify_func) _notify_node;
        notify->arg = e;

        e->node->flags = XL_DAGC_FLAG_WAIT_EVAL;

        /* Create a child environment to execute the function in. */
        child_env = calloc(1, sizeof(struct ubik_env));
        err = ubik_env_make_child(child_env, e->env);
        if (err != OK)
                return err;

        /* Take a reference here that will be released in _notify_node */
        err = ubik_take(e->node->known.graph);
        if (err != OK)
                return err;

        err = ubik_schedule_push(s, e->node->known.graph, child_env, notify);
        if (err != OK)
                return err;

        return OK;
}

no_ignore static bool
_is_ready(struct ubik_exec_unit *e)
{
        return !(e->node->flags & XL_DAGC_WAIT_MASK);
}

no_ignore static ubik_error
_dump_exec_unit(struct ubik_exec_unit *u)
{
        struct ubik_dagc_node *d1, *d2, *d3;
        char *buf;
        ubik_error err;

        buf = ubik_node_explain(u->node);
        printf("\t%s\n", buf);
        free(buf);
        printf("\t\tenv @%hx ", (short)((uintptr_t) u->env));
        printf("parent @%hx\n", (short)((uintptr_t) u->env->parent));

        printf("\t\twait on d1 %d d2 %d d3 %d eval %d data %d\n",
                !!(u->node->flags & XL_DAGC_FLAG_WAIT_D1),
                !!(u->node->flags & XL_DAGC_FLAG_WAIT_D2),
                !!(u->node->flags & XL_DAGC_FLAG_WAIT_D3),
                !!(u->node->flags & XL_DAGC_FLAG_WAIT_EVAL),
                !!(u->node->flags & XL_DAGC_FLAG_WAIT_DATA));

        err = ubik_dagc_get_deps(&d1, &d2, &d3, u->node);
        if (err != OK)
                return err;

        if (d1 != NULL)
        {
                buf = ubik_node_explain(d1);
                printf("\t\td1: %s\n", buf);
                free(buf);
        }

        if (d2 != NULL)
        {
                buf = ubik_node_explain(d2);
                printf("\t\td2: %s\n", buf);
                free(buf);
        }

        if (d3 != NULL)
        {
                buf = ubik_node_explain(d3);
                printf("\t\td3: %s\n", buf);
                free(buf);
        }
        return OK;
}

no_ignore ubik_error
ubik_schedule_dump(struct ubik_scheduler *s)
{
        struct ubik_exec_unit *u;
        ubik_error err;

        printf("scheduler dump\nwaiting jobs:\n");
        u = s->wait;
        while (u != NULL)
        {
                err = _dump_exec_unit(u);
                if (err != OK)
                        return err;
                u = u->next;
        }

        printf("ready jobs:\n");
        u = s->ready;
        while (u != NULL)
        {
                err = _dump_exec_unit(u);
                if (err != OK)
                        return err;
                u = u->next;
        }

        return OK;
}

/* Runs a single pass of the scheduler. */
no_ignore static ubik_error
_run_single_pass(struct ubik_scheduler *s)
{
        struct ubik_exec_unit *u, *t;
        struct ubik_exec_unit *prev;
        ubik_error err;

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
#ifdef XL_SCHEDULE_DEBUG
                        printf("moving %s from waiting to ready\n",
                               ubik_node_explain(u->node));
#endif
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
        {
                err = ubik_schedule_dump(s);
                if (err != OK)
                        return err;
                return ubik_raise(ERR_DEADLOCK, "all jobs are waiting");
        }

#ifdef XL_SCHEDULE_STEP
        err = ubik_schedule_dump(s);
        if (err != OK)
                return err;
#endif

        /* Now all of the ready jobs are in the ready pile, so we just have to
         * execute them. */
        u = s->ready;
        while (u != NULL)
        {
                err = ubik_dagc_node_eval(u->env, u->node);
                if (err != OK)
                        return err;

                s->ready = s->ready->next;

                if (u->node->flags & XL_DAGC_FLAG_COMPLETE &&
                        *u->node->known.tag & TAG_GRAPH &&
                        u->node->known.graph->in_arity == 0)
                {
#ifdef XL_SCHEDULE_DEBUG
                        printf("collapsing %s\n",
                               ubik_node_explain(u->node));
#endif
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
#ifdef XL_SCHEDULE_DEBUG
                        printf("marking %s complete\n",
                               ubik_node_explain(u->node));
#endif
                        err = ubik_schedule_complete(s, u);
                        if (err != OK)
                                return err;
                }
                else if (u->node->flags & XL_DAGC_WAIT_MASK)
                {
#ifdef XL_SCHEDULE_DEBUG
                        printf("moving %s back to waiting\n",
                               ubik_node_explain(u->node));
#endif
                        u->next = s->wait;
                        s->wait = u;

                        err = _push_deps(s, u->graph, u->node, u->env);
                        if (err != OK)
                                return err;
                }
                else return ubik_raise(
                        ERR_BAD_HEADER,
                        "eval'ed node is not complete or waiting");

                u = s->ready;
        }

        return OK;
}

/* Runs all queued jobs on the scheduler. */
no_ignore ubik_error
ubik_schedule_run(struct ubik_scheduler *s)
{
        ubik_error err;

        while (s->wait != NULL || s->ready != NULL)
        {
                err = _run_single_pass(s);
                if (err != OK)
                        return err;
        }
        return OK;
}
