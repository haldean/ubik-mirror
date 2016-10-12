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
#include "ubik/env.h"
#include "ubik/eval.h"
#include "ubik/fun.h"
#include "ubik/schedule.h"
#include "ubik/util.h"

struct ubik_scheduler
{
        struct ubik_exec_unit *wait;
        struct ubik_exec_unit *ready;
};

static inline struct ubik_value *
get_fun(struct ubik_value *v)
{
        if (v->type == UBIK_PAP)
                return v->pap.base_func;
        ubik_assert(v->type == UBIK_FUN);
        return v;
}

static void
free_exec_graph(struct ubik_exec_graph *gexec)
{
        unused(gexec);
        ubik_assert("not implemented!" == NULL);
}

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
_set_initial_ready(
        struct ubik_exec_graph *gexec,
        ubik_word node_id)
{
        struct ubik_value *graph;
        ubik_word d1, d2, d3;
        ubik_word node_type;
        ubik_error err;

        graph = get_fun(gexec->v);
        node_type = graph->fun.nodes[node_id].node_type;

        /* Input nodes are special; they're only ready once their values
         * have been filled in, even though they have no dependencies.
         * Only application of the graph they participate in can make
         * them ready, so these don't changed here. */
        if (node_type == UBIK_INPUT)
                return OK;

        err = ubik_fun_get_deps(&d1, &d2, &d3, &graph->fun.nodes[node_id]);
        if (err != OK)
                return err;

        /* Cond nodes are also special; they start out only waiting on
         * their condition, and then on the basis of their condition
         * they are re-waited. */
        if (node_type == UBIK_COND)
        {
                if (gexec->status[d1] & UBIK_STATUS_COMPLETE)
                        gexec->status[node_id] = UBIK_STATUS_READY;
                else
                        gexec->status[node_id] = UBIK_STATUS_WAIT_D1;
        }
        else
        {
                gexec->status[node_id] = UBIK_STATUS_READY;
                if (d1 != UBIK_INVALID_NODE_ID)
                        if (!(gexec->status[d1] & UBIK_STATUS_COMPLETE))
                                gexec->status[node_id] |= UBIK_STATUS_WAIT_D1;
                if (d2 != UBIK_INVALID_NODE_ID)
                        if (!(gexec->status[d2] & UBIK_STATUS_COMPLETE))
                                gexec->status[node_id] |= UBIK_STATUS_WAIT_D2;
                if (d3 != UBIK_INVALID_NODE_ID)
                        if (!(gexec->status[d3] & UBIK_STATUS_COMPLETE))
                                gexec->status[node_id] |= UBIK_STATUS_WAIT_D3;
        }

        return OK;
}

no_ignore static ubik_error
_enqueue(
        struct ubik_scheduler *s,
        struct ubik_exec_graph *gexec,
        ubik_word node)
{
        struct ubik_exec_unit *u;
        ubik_error err;
        struct ubik_exec_unit *test;

        /* Check to make sure this node isn't already enqueued. */
        for (test = s->wait; test != NULL; test = test->next)
        {
                if (test->gexec->v != gexec->v)
                        continue;
                if (test->node != node)
                        continue;
                return OK;
        }
        for (test = s->ready; test != NULL; test = test->next)
        {
                if (test->gexec->v != gexec->v)
                        continue;
                if (test->node != node)
                        continue;
                return OK;
        }

        u = calloc(1, sizeof(struct ubik_exec_unit));
        if (u == NULL)
                return ubik_raise(ERR_NO_MEMORY, "exec unit alloc");
        u->node = node;
        u->gexec = gexec;

        err = _set_initial_ready(gexec, node);
        if (err != OK)
                return err;

        u->next = s->wait;
        s->wait = u;

        return OK;
}

no_ignore static ubik_error
_eval_native_dagc(
        struct ubik_scheduler *s,
        struct ubik_exec_graph *gexec)
{
        struct ubik_value *graph;
        ubik_error err;
        struct ubik_exec_unit unit;

        graph = get_fun(gexec->v);
        err = graph->fun.evaluator(gexec);
        if (err != OK)
                return err;

        gexec->status[graph->fun.result] = UBIK_STATUS_COMPLETE;

        if (gexec->notify == NULL)
                return OK;

        unit.node = graph->fun.result;
        unit.gexec = gexec;
        unit.next = NULL;

        err = gexec->notify->notify(gexec->notify->arg, s, &unit);
        if (err != OK)
                return err;

        return OK;
}

no_ignore static ubik_error
_push_dep_tree(
        struct ubik_scheduler *s,
        struct ubik_exec_graph *gexec,
        ubik_word node);

/* Enqueues the nodes on which the provided node is waiting. */
no_ignore static ubik_error
_push_deps(
        struct ubik_scheduler *s,
        struct ubik_exec_graph *gexec,
        ubik_word node)
{
        struct ubik_value *graph;
        ubik_word d1, d2, d3;
        ubik_error err;

        graph = get_fun(gexec->v);
        err = ubik_fun_get_deps(&d1, &d2, &d3, &graph->fun.nodes[node]);
        if (err != OK)
                return err;

        ubik_assert(d1 != node);
        ubik_assert(d2 != node);
        ubik_assert(d3 != node);

        if (d1 != UBIK_INVALID_NODE_ID
            && (gexec->status[node] & UBIK_STATUS_WAIT_D1))
        {
                /* only the root gets notified; everything below it doesn't */
                err = _push_dep_tree(s, gexec, d1);
                if (err != OK)
                        return err;
        }
        if (d2 != UBIK_INVALID_NODE_ID
            && (gexec->status[node] & UBIK_STATUS_WAIT_D2))
        {
                err = _push_dep_tree(s, gexec, d2);
                if (err != OK)
                        return err;
        }
        if (d3 != UBIK_INVALID_NODE_ID
            && (gexec->status[node] & UBIK_STATUS_WAIT_D3))
        {
                err = _push_dep_tree(s, gexec, d3);
                if (err != OK)
                        return err;
        }

        return OK;
}

/* Enqueues a node and all applicable dependencies. */
no_ignore static ubik_error
_push_dep_tree(
        struct ubik_scheduler *s,
        struct ubik_exec_graph *gexec,
        ubik_word node)
{
        ubik_error err;

        err = _enqueue(s, gexec, node);
        if (err != OK)
                return err;

        err = _push_deps(s, gexec, node);
        if (err != OK)
                return err;

        return OK;
}

/* Pushes a graph into the scheduler for execution. */
no_ignore ubik_error
ubik_schedule_push(
        struct ubik_scheduler *s,
        struct ubik_value *val,
        struct ubik_env *env,
        struct ubik_exec_notify *notify,
        struct ubik_workspace *workspace)
{
        ubik_error err;
        struct ubik_exec_graph *gexec;
        struct ubik_value *graph;
        size_t i;

        graph = get_fun(val);
        gexec = calloc(1, sizeof(struct ubik_exec_graph));
        gexec->v = val;
        gexec->status = calloc(graph->fun.n, sizeof(*gexec->status));
        gexec->nv = calloc(graph->fun.n, sizeof(struct ubik_value *));
        gexec->nt = calloc(graph->fun.n, sizeof(struct ubik_value *));
        gexec->env = env;
        gexec->notify = notify;
        gexec->workspace = workspace;

        /* Graphs with special evaluators get to cheat and skip all this biz. */
        if (graph->fun.evaluator != NULL)
        {
                err = _eval_native_dagc(s, gexec);
                if (err != OK)
                        return err;
                free_exec_graph(gexec);
        }

        for (i = 0; i < graph->fun.n; i++)
        {
                err = _set_initial_ready(gexec, i);
                if (err != OK)
                        return err;
        }
        for (i = 0; i < graph->fun.n; i++)
        {
                if (!graph->fun.nodes[i].is_terminal)
                        continue;
                err = _push_dep_tree(s, gexec, i);
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
        local(vector) struct ubik_vector parents = {0};
        struct ubik_value *graph;
        ubik_word d1, d2, d3, p;
        size_t i;
        ubik_error err;
        bool done;

        graph = get_fun(e->gexec->v);
        err = ubik_fun_get_parents(&parents, graph, e->node);
        if (err != OK)
                return err;

        for (i = 0; i < parents.n; i++)
        {
                p = (ubik_word) parents.elems[i];
                err = ubik_fun_get_deps(&d1, &d2, &d3, &graph->fun.nodes[p]);
                if (err != OK)
                        return err;

                if (d1 == e->node)
                        e->gexec->status[p] &= ~UBIK_STATUS_WAIT_D1;
                if (d2 == e->node)
                        e->gexec->status[p] &= ~UBIK_STATUS_WAIT_D2;
                if (d3 == e->node)
                        e->gexec->status[p] &= ~UBIK_STATUS_WAIT_D3;
        }

        /* If this was a terminal node, it's possible that we're done with this
           graph. Check if there are any outstanding terminals, and if there
           aren't, clean up the graph executor and notify listeners. */
        if (graph->fun.nodes[e->node].is_terminal)
        {
                done = true;
                for (i = 0; i < graph->fun.n && done; i++)
                        if (i != e->node && graph->fun.nodes[i].is_terminal)
                                done = false;
                if (done)
                {
                        if (e->gexec->notify != NULL) {
                                err = e->gexec->notify->notify(
                                        e->gexec->notify->arg, s, e);
                                if (err != OK)
                                        return err;
                        }
                        err = ubik_env_free(e->gexec->env);
                        if (err != OK)
                                return err;
                        free(e->gexec->nv);
                        free(e->gexec->nt);
                        free(e->gexec->status);
                }
        }

        free(e);
        return OK;
}

ubik_error
_notify_node(
        struct ubik_exec_unit *waiting,
        struct ubik_scheduler *s,
        struct ubik_exec_unit *complete)
{
        ubik_error err;

#ifdef UBIK_SCHEDULE_DEBUG
        printf("notifying %d on completion of %d\n",
               waiting->node, complete->node);
#endif

        waiting->gexec->nv[waiting->node] = complete->gexec->nv[complete->node];
        waiting->gexec->nt[waiting->node] = complete->gexec->nt[complete->node];
        waiting->gexec->status[waiting->node] = UBIK_STATUS_COMPLETE;

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

#if UBIK_SCHEDULE_DEBUG
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

        e->gexec->status[e->node] = UBIK_STATUS_WAIT_EVAL;

        /* Create a child environment to execute the function in. */
        child_env = calloc(1, sizeof(struct ubik_env));
        err = ubik_env_make_child(child_env, e->gexec->env);
        if (err != OK)
                return err;

        err = ubik_schedule_push(
                s, e->gexec->nv[e->node], child_env, notify,
                e->gexec->workspace);
        if (err != OK)
                return err;

        return OK;
}

no_ignore static bool
is_ready(struct ubik_exec_unit *e)
{
        return !(e->gexec->status[e->node] & UBIK_STATUS_WAIT_MASK);
}

no_ignore static bool
can_collapse(struct ubik_exec_unit *e)
{
        struct ubik_value *v;
        ubik_word arity;
        if (e->gexec->status[e->node] != UBIK_STATUS_COMPLETE)
                return false;
        v = e->gexec->nv[e->node];
        if (v->type == UBIK_FUN)
                return v->fun.arity == 0;
        if (v->type != UBIK_PAP)
                return false;
        for (arity = 0; v->type == UBIK_PAP; arity++, v = v->pap.func);
        ubik_assert(v->type == UBIK_FUN);
        return arity == v->fun.arity;
}

no_ignore static ubik_error
_dump_exec_unit(struct ubik_exec_unit *u)
{
        /*
        struct ubik_dagc_node *d1, *d2, *d3;
        char *buf;
        ubik_error err;

        buf = ubik_node_explain(u->node);
        printf("\t%s\n", buf);
        free(buf);
        printf("\t\tenv @%hx ", (short)((uintptr_t) u->env));
        printf("parent @%hx\n", (short)((uintptr_t) u->env->parent));

        printf("\t\twait on d1 %d d2 %d d3 %d eval %d data %d\n",
                !!(u->node->flags & UBIK_STATUS_WAIT_D1),
                !!(u->node->flags & UBIK_STATUS_WAIT_D2),
                !!(u->node->flags & UBIK_STATUS_WAIT_D3),
                !!(u->node->flags & UBIK_STATUS_WAIT_EVAL),
                !!(u->node->flags & UBIK_STATUS_WAIT_DATA));

        err = ubik_fun_get_deps(&d1, &d2, &d3, u->node);
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
        */
        unused(u);
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
        ubik_word status;
        ubik_error err;

        /* This proceeds in two phases; first, we move everything that is ready
         * to be executed from the wait pile to the ready pile, then we execute
         * everything in the ready pile. */
        u = s->wait;
        prev = NULL;
        while (u != NULL)
        {
                t = u->next;

                if (is_ready(u))
                {
#ifdef UBIK_SCHEDULE_DEBUG
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

#ifdef UBIK_SCHEDULE_STEP
        err = ubik_schedule_dump(s);
        if (err != OK)
                return err;
#endif

        /* Now all of the ready jobs are in the ready pile, so we just have to
         * execute them. */
        u = s->ready;
        while (u != NULL)
        {
                err = ubik_node_eval(u);
                if (err != OK)
                        return err;

                s->ready = s->ready->next;

                status = u->gexec->status[u->node];
                if (can_collapse(u))
                {
#ifdef UBIK_SCHEDULE_DEBUG
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
                else if (status & UBIK_STATUS_COMPLETE)
                {
#ifdef UBIK_SCHEDULE_DEBUG
                        printf("marking %s complete\n",
                               ubik_node_explain(u->node));
#endif
                        err = ubik_schedule_complete(s, u);
                        if (err != OK)
                                return err;
                }
                else if (status & UBIK_STATUS_WAIT_MASK)
                {
#ifdef UBIK_SCHEDULE_DEBUG
                        printf("moving %s back to waiting\n",
                               ubik_node_explain(u->node));
#endif
                        u->next = s->wait;
                        s->wait = u;

                        err = _push_deps(s, u->gexec, u->node);
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

no_ignore ubik_error
ubik_schedule_push_roots(
        struct ubik_scheduler *s,
        struct ubik_env *env,
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
                        err = ubik_schedule_push(
                                s, &ws->values[i], env, NULL, ws);
                        if (err != OK)
                                return err;
                }
        }

        return OK;
}
