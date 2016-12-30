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

#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "ubik/assert.h"
#include "ubik/env.h"
#include "ubik/eval.h"
#include "ubik/fun.h"
#include "ubik/pointerset.h"
#include "ubik/rwlock.h"
#include "ubik/schedule.h"
#include "ubik/util.h"
#include "ubik/value.h"
#include "ubik/vector.h"

#define UBIK_SCHEDULE_STEP 0

struct ubik_scheduler
{
        struct ubik_exec_unit *wait;
        struct ubik_exec_unit *ready;
        /* all accesses of wait and ready must be holding this lock */
        struct ubik_rwlock queue_lock;
};

struct ubik_value *
get_fun(struct ubik_value *v)
{
        if (v->type == UBIK_PAP)
                return v->pap.base_func;
        ubik_assert(v->type == UBIK_FUN);
        return v;
}

void
free_exec_graph(struct ubik_exec_graph *gexec)
{
        if (gexec->transient_env)
        {
                ubik_assert(ubik_env_free(gexec->env) == OK);
                free(gexec->env);
        }
        if (gexec->notify != NULL)
                free(gexec->notify);
        ubik_rwlock_destroy(&gexec->lock);
        free(gexec->nv);
        free(gexec->nt);
        free(gexec->status);
        free(gexec);
}

/* Creates a scheduler. */
no_ignore ubik_error
ubik_schedule_new(struct ubik_scheduler **s)
{
        ubik_galloc1(s, struct ubik_scheduler);
        ubik_rwlock_init(&(*s)->queue_lock);
        return OK;
}

/* Destroys a scheduler. */
no_ignore ubik_error
ubik_schedule_free(struct ubik_scheduler *s)
{
        struct ubik_exec_unit *to_free;
        struct ubik_vector gexecs = {0};
        ubik_error err;
        size_t i;

        ubik_rwlock_write(&s->queue_lock);

        while (s->wait != NULL)
        {
                to_free = s->wait;
                s->wait = s->wait->next;
                err = ubik_pointer_set_add(NULL, &gexecs, to_free->gexec);
                if (err != OK)
                        return err;
                free(to_free);
        }

        while (s->ready != NULL)
        {
                to_free = s->ready;
                s->ready = s->ready->next;
                err = ubik_pointer_set_add(NULL, &gexecs, to_free->gexec);
                if (err != OK)
                        return err;
                free(to_free);
        }

        for (i = 0; i < gexecs.n; i++)
        {
                free_exec_graph(gexecs.elems[i]);
        }
        ubik_vector_free(&gexecs);

        ubik_rwlock_release(&s->queue_lock);
        ubik_rwlock_destroy(&s->queue_lock);
        return OK;
}

/* Initializes the flags of a given node. Must be called with the gexec lock
 * held. */
no_ignore ubik_error
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

no_ignore ubik_error
_enqueue(
        struct ubik_scheduler *s,
        struct ubik_exec_graph *gexec,
        ubik_word node)
{
        struct ubik_exec_unit *u;
        ubik_error err;
        struct ubik_exec_unit *test;

        ubik_rwlock_write_scope(&s->queue_lock);

        /* Check to make sure this node isn't already enqueued. */
        for (test = s->wait; test != NULL; test = test->next)
        {
                if (test->gexec->v != gexec->v)
                        continue;
                if (test->node != node)
                        continue;
                return ubik_raise(ERR_PRESENT, "already enqueued");
        }
        for (test = s->ready; test != NULL; test = test->next)
        {
                if (test->gexec->v != gexec->v)
                        continue;
                if (test->node != node)
                        continue;
                return OK;
        }

        ubik_galloc1(&u, struct ubik_exec_unit);
        u->node = node;
        u->gexec = gexec;

        {
                ubik_rwlock_write_scope(&gexec->lock);
                gexec->refcount++;

                err = _set_initial_ready(gexec, node);
                if (err != OK)
                        return err;
        }

        u->next = s->wait;
        s->wait = u;

        return OK;
}

/* Evaluates a native graph. Must be called with the gexec lock held. */
no_ignore ubik_error
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

no_ignore ubik_error
_push_dep_tree(
        struct ubik_scheduler *s,
        struct ubik_exec_graph *gexec,
        ubik_word node);

/* Enqueues the nodes on which the provided node is waiting. */
no_ignore ubik_error
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
no_ignore ubik_error
_push_dep_tree(
        struct ubik_scheduler *s,
        struct ubik_exec_graph *gexec,
        ubik_word node)
{
        ubik_error err;

        err = _enqueue(s, gexec, node);
        if (err != OK) {
                if (err->error_code == ERR_PRESENT) {
                        free(err);
                        return OK;
                }
                return err;
        }

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
        bool transient_env,
        struct ubik_exec_notify *notify,
        struct ubik_workspace *workspace)
{
        ubik_error err;
        struct ubik_exec_graph *gexec;
        struct ubik_value *graph;
        struct ubik_value *pap;
        size_t i;
        size_t j;
        char *arginfo;
        size_t arginfo_len;

        graph = get_fun(val);
        ubik_galloc1(&gexec, struct ubik_exec_graph);
        ubik_galloc((void **) &gexec->status,
                    graph->fun.n, sizeof(*gexec->status));
        ubik_galloc((void **) &gexec->nv,
                    graph->fun.n, sizeof(struct ubik_value *));
        ubik_galloc((void **) &gexec->nt,
                    graph->fun.n, sizeof(struct ubik_value *));
        gexec->v = val;
        gexec->env = env;
        gexec->notify = notify;
        gexec->workspace = workspace;
        gexec->refcount = 0;
        gexec->transient_env = transient_env;
        ubik_rwlock_init(&gexec->lock);

        /* Add types and values for inputs to the executor */
        for (i = graph->fun.arity - 1, pap = val;
             pap->type == UBIK_PAP;
             pap = pap->pap.func, i--)
        {
                for (j = 0; j < graph->fun.n; j++)
                {
                        if (graph->fun.nodes[j].node_type != UBIK_INPUT)
                                continue;
                        if (graph->fun.nodes[j].input.arg_num != i)
                                continue;
                        gexec->nv[j] = pap->pap.arg;
                        gexec->nt[j] = pap->pap.arg_type;
                        break;
                }
        }

        if (unlikely(graph->gc.traced))
        {
                if (graph->dbg.used)
                {
                        printf("calling traced value %s, rev-args:",
                               graph->dbg.name);
                        pap = val;
                        while (pap->type == UBIK_PAP)
                        {
                                err = ubik_value_humanize(
                                        &arginfo, &arginfo_len, pap->pap.arg);
                                if (err != OK)
                                        return err;
                                printf(" %s", arginfo);
                                pap = pap->pap.func;
                        }
                        printf("\n");
                }
                else
                        printf("calling traced value, no debugging info\n");
        }

        /* Graphs with special evaluators get to cheat and skip all this biz. */
        if (graph->fun.evaluator != NULL)
        {
                err = _eval_native_dagc(s, gexec);
                free_exec_graph(gexec);
                return err;
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

        {
                ubik_rwlock_write_scope(&e->gexec->lock);
                for (i = 0; i < parents.n; i++)
                {
                        p = (ubik_word) parents.elems[i];
                        err = ubik_fun_get_deps(
                                &d1, &d2, &d3, &graph->fun.nodes[p]);
                        if (err != OK)
                                return err;

                        if (d1 == e->node)
                                e->gexec->status[p] &= ~UBIK_STATUS_WAIT_D1;
                        if (d2 == e->node)
                                e->gexec->status[p] &= ~UBIK_STATUS_WAIT_D2;
                        if (d3 == e->node)
                                e->gexec->status[p] &= ~UBIK_STATUS_WAIT_D3;
                }
        }

        /* If this was a terminal node, it's possible that we're done with this
           graph. Check if there are any outstanding terminals, and if there
           aren't, notify listeners. */
        if (graph->fun.nodes[e->node].is_terminal)
        {
                /* Hold an exclusive lock on this section, to prevent
                 * double-notifying races. */
                ubik_rwlock_write(&e->gexec->lock);
                done = true;
                for (i = 0; i < graph->fun.n && done; i++)
                        if (i != e->node && graph->fun.nodes[i].is_terminal)
                                done &= e->gexec->status[i]
                                        == UBIK_STATUS_COMPLETE;
                ubik_rwlock_release(&e->gexec->lock);

                if (done)
                {
                        if (e->gexec->notify != NULL) {
                                err = e->gexec->notify->notify(
                                        e->gexec->notify->arg, s, e);
                                if (err != OK)
                                        return err;
                        }
                }
        }

        {
                ubik_rwlock_write_scope(&e->gexec->lock);
                ubik_assert(e->gexec->refcount > 0);
                done = --e->gexec->refcount == 0;
        }

        if (done)
                free_exec_graph(e->gexec);
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

        {
                ubik_rwlock_write(&waiting->gexec->lock);
                ubik_rwlock_read(&complete->gexec->lock);

                waiting->gexec->nv[waiting->node] =
                        complete->gexec->nv[complete->node];
                waiting->gexec->nt[waiting->node] =
                        complete->gexec->nt[complete->node];
                waiting->gexec->status[waiting->node] = UBIK_STATUS_COMPLETE;

                /* Results of traced functions are also traced. */
                if (complete->gexec->v->gc.traced)
                {
                        waiting->gexec->nv[waiting->node]->dbg =
                                complete->gexec->v->dbg;
                        waiting->gexec->nv[waiting->node]->dbg.nofree = true;
                        waiting->gexec->nv[waiting->node]->gc.traced = true;
                }

                ubik_rwlock_release(&waiting->gexec->lock);
                ubik_rwlock_release(&complete->gexec->lock);
        }

        err = ubik_schedule_complete(s, waiting);
        if (err != OK)
                return err;

        return OK;
}

no_ignore ubik_error
_collapse_graph(
        struct ubik_scheduler *s,
        struct ubik_exec_unit *e)
{
        struct ubik_exec_notify *notify;
        struct ubik_env *child_env;
        struct ubik_value *v;
        ubik_error err;

        ubik_galloc1(&notify, struct ubik_exec_notify);
        notify->notify = (ubik_exec_notify_func) _notify_node;
        notify->arg = e;

        {
                ubik_rwlock_write_scope(&e->gexec->lock);
                e->gexec->status[e->node] = UBIK_STATUS_WAIT_EVAL;
                v = e->gexec->nv[e->node];
        }

        /* Create a child environment to execute the function in. */
        ubik_galloc1(&child_env, struct ubik_env);
        err = ubik_env_make_child(child_env, e->gexec->env);
        if (err != OK)
                return err;

        /* Note: this can end up synchronously notifying on the node if the
         * requested collapse calls a native function, so it can't be called
         * with a lock held (notifying the node reasonably requires a write
         * lock to be held. This is why we pull the value out before we call
         * this function. */
        err = ubik_schedule_push(
                s, v, child_env, true, notify, e->gexec->workspace);
        if (err != OK)
                return err;

        return OK;
}

no_ignore bool
is_ready(struct ubik_exec_unit *e)
{
        ubik_rwlock_read_scope(&e->gexec->lock);
        return !(e->gexec->status[e->node] & UBIK_STATUS_WAIT_MASK);
}

no_ignore bool
can_collapse(struct ubik_exec_unit *e)
{
        struct ubik_value *v;
        ubik_word arity;

        ubik_rwlock_read_scope(&e->gexec->lock);

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

no_ignore ubik_error
_dump_exec_unit(struct ubik_exec_unit *u)
{
        ubik_rwlock_read_scope(&u->gexec->lock);

        printf("\tvalue %08" PRIx64 " node %03" PRIx64 " ",
               get_fun(u->gexec->v)->gc.id, u->node);

        uint8_t status = u->gexec->status[u->node];
        printf("wait d1 %d d2 %d d3 %d eval %d data %d ",
               !!(status & UBIK_STATUS_WAIT_D1),
               !!(status & UBIK_STATUS_WAIT_D2),
               !!(status & UBIK_STATUS_WAIT_D3),
               !!(status & UBIK_STATUS_WAIT_EVAL),
               !!(status & UBIK_STATUS_WAIT_DATA));

        printf("env %04" PRIx16 " parent %04" PRIx16 "\n",
               (uint16_t) ((uintptr_t) u->gexec->env),
               (uint16_t) ((uintptr_t) u->gexec->env->parent));

        return OK;
}

no_ignore ubik_error
ubik_schedule_dump(struct ubik_scheduler *s)
{
        struct ubik_exec_unit *u;
        ubik_error err;

        printf("\nscheduler dump\nwaiting jobs:\n");
        u = s->wait;
        while (u != NULL)
        {
                err = _dump_exec_unit(u);
                if (err != OK)
                        return err;
                u = u->next;
        }

        printf("\nready jobs:\n");
        u = s->ready;
        while (u != NULL)
        {
                err = _dump_exec_unit(u);
                if (err != OK)
                        return err;
                u = u->next;
        }
        printf("\n");

        return OK;
}

/* Runs a single pass of the scheduler. */
no_ignore ubik_error
_run_single_pass(struct ubik_scheduler *s)
{
        struct ubik_exec_unit *u, *t, *next_exec;
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

#if UBIK_SCHEDULE_STEP
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

                next_exec = s->ready->next;

                status = u->gexec->status[u->node];
                if (can_collapse(u))
                {
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
                        err = ubik_schedule_complete(s, u);
                        if (err != OK)
                                return err;
                }
                else if (status & UBIK_STATUS_WAIT_MASK)
                {
                        u->next = s->wait;
                        s->wait = u;

                        err = _push_deps(s, u->gexec, u->node);
                        if (err != OK)
                                return err;
                }
                else return ubik_raise(
                        ERR_BAD_HEADER,
                        "eval'ed node is not complete or waiting");

                /* It's important we do this at the end; we want u to be
                 * reachable when we free the environment if we end up
                 * early-returning because of an error. */
                s->ready = next_exec;
                u = next_exec;
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
                                s, &ws->values[i], env, false, NULL, ws);
                        if (err != OK)
                                return err;
                }
        }

        return OK;
}
