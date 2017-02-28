/*
 * evaluator.c: evalutes functions in a queue
 * Copyright (C) 2017, Haldean Brown
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

#include <pthread.h>
#include <stdatomic.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "ubik/alloc.h"
#include "ubik/assert.h"
#include "ubik/evaluator.h"
#include "ubik/jobq.h"
#include "ubik/rttypes.h"
#include "ubik/ubik.h"
#include "ubik/value.h"

enum node_status
{
        WAIT,
        DONE,
        LOADED,
        DATA,
        APPLY,
        NEEDED,
};

#define status(e, i) \
        atomic_load_explicit(&e->s[i], memory_order_acquire)
#define setstatus(e, i, new) \
        atomic_store_explicit(&e->s[i], new, memory_order_release)

struct ubik_evaluator
{
        struct ubik_jobq q;
        struct ubik_env *env;
        struct ubik_workspace *ws;
        size_t n_workers;
        atomic_bool die;
};

struct ubik_eval_req
{
        struct ubik_eval_state *e;
        struct ubik_eval_state *waiting;
        size_t node;
        struct ubik_evaluate_callback *cb;
};

struct ubik_eval_state
{
        struct ubik_value *f;
        enum node_status *s;
        struct ubik_value **args;
        struct ubik_value **argtypes;
        struct ubik_value **nv;
        struct ubik_value **nt;
        size_t n;
        size_t term;
        pthread_mutex_t lock;
};

struct eval_thread_state
{
        struct ubik_evaluator *e;
        size_t id;
};

void
free_eval_state(struct ubik_eval_state *e)
{
        free(e->args);
        free(e->argtypes);
        free(e->nv);
        free(e->nt);
        free(e->s);
}

no_ignore static ubik_error
push(
        struct ubik_evaluator *evaluator,
        struct ubik_value *v,
        struct ubik_eval_state *waiting,
        size_t wait_node,
        size_t worker_id,
        struct ubik_evaluate_callback *cb);

struct node_ref
{
        struct ubik_eval_state *e;
        size_t node;
};

no_ignore static ubik_error
_load_callback(
        void *node_ref_void,
        struct ubik_env *env,
        struct ubik_uri *uri)
{
        struct node_ref *r;
        unused(env);
        unused(uri);

        /* Can't actually get the value here, because you aren't allowed to
         * access the env from an env callback (deadlocks!). We mark the node
         * as WAIT again, so the next pass will catch it. */
        r = node_ref_void;
#ifdef EVALUATOR_DEBUG
        printf("got data for %lu/%lu\n", (uintptr_t) r->e, r->node);
        fflush(stdout);
#endif
        setstatus(r->e, r->node, WAIT);
        free(r);

        return OK;
}

static inline void
run_cond(struct ubik_eval_state *e, size_t i, struct ubik_node *node)
{
        size_t t;
        enum node_status is, ts;

        t = node->cond.condition;
        is = status(e, i);
        ts = status(e, t);

        if (ts == WAIT)
        {
                if (is == NEEDED)
                        setstatus(e, t, NEEDED);
                return;
        }
        if (ts != DONE)
                return;

        ubik_assert(e->nv[t]->type == UBIK_BOO);
        t = e->nv[t]->boo.value
                ? node->cond.if_true
                : node->cond.if_false;
        ts = status(e, t);
        if (ts == WAIT)
        {
                if (is == NEEDED)
                        setstatus(e, t, NEEDED);
                return;
        }
        if (ts != DONE)
                return;
        e->nv[i] = e->nv[t];
        e->nt[i] = e->nt[t];
        setstatus(e, i, DONE);
}

no_ignore static inline ubik_error
run_apply(
        struct ubik_evaluator *evaluator,
        struct ubik_eval_state *e,
        size_t i,
        struct ubik_node *node)
{
        size_t f;
        size_t a;
        struct ubik_value *r;
        enum node_status is;
        enum node_status fs;
        enum node_status as;
        ubik_error err;

        /* Don't evaluate any applications unless we need to; without this,
         * we'll end up doing tons of extra work potentially infinitely
         * evaluating the alternate sides of conditionals. */
        is = status(e, i);
        if (is != NEEDED)
                return OK;

        f = node->apply.func;
        a = node->apply.arg;

        fs = status(e, f);
        as = status(e, a);

        if (fs == WAIT)
                setstatus(e, f, NEEDED);
        if (as == WAIT)
                setstatus(e, a, NEEDED);
        if (fs != DONE || as != DONE)
                return OK;

        err = ubik_value_new(&r, evaluator->ws);
        if (err != OK)
                return err;

        r->type = UBIK_PAP;
        r->pap.func = e->nv[f];
        if (r->pap.func->type == UBIK_FUN)
                r->pap.base_func = r->pap.func;
        else if (r->pap.func->type == UBIK_PAP)
                r->pap.base_func = r->pap.func->pap.base_func;
        r->pap.arg = e->nv[a];
        r->pap.arg_type = e->nt[a];

        /* PAPs inherit the traced flag from their base
         * function, so the tracer doesn't have to crawl back
         * up to the base function to see if the function is
         * being traced. */
        r->gc.traced = r->pap.base_func->gc.traced;

        err = ubik_value_new(&e->nt[i], evaluator->ws);
        if (err != OK)
                return err;

        e->nv[i] = r;
        err = ubik_type_func_apply(
                        e->nt[i], e->nt[f], e->nt[a]);
        if (err != OK)
        {
                /* TODO: runtime types shouldn't be silently
                 * ignored! */
                free(err);
        }
        setstatus(e, i, DONE);
        return OK;
}

no_ignore static inline ubik_error
run_load(
        struct ubik_evaluator *evaluator,
        struct ubik_eval_state *e,
        size_t i,
        struct ubik_node *node)
{
        struct node_ref *node_ref;
        enum node_status is;
        ubik_error err;

        is = status(e, i);
        if (is != WAIT && is != NEEDED)
                return OK;

        err = ubik_env_get(
                        &e->nv[i],
                        &e->nt[i],
                        evaluator->env,
                        node->load.loc);
        if (err == OK)
        {
                setstatus(e, i, DONE);
                return OK;
        }

        /* native funcs never appear later. */
        if (node->load.loc->scope == SCOPE_NATIVE &&
                        err->error_code == ERR_ABSENT)
        {
                char *buf = ubik_uri_explain(node->load.loc);
                printf("access nonexistent native function %s\n", buf);
                free(buf);
                return err;
        }

        if (err->error_code != ERR_ABSENT)
                return err;

        free(err);
        if (is != NEEDED)
                return OK;

        /* file a wait request if we know we need this node. */
        setstatus(e, i, DATA);

        ubik_alloc1(&node_ref, struct node_ref, NULL);
        node_ref->e = e;
        node_ref->node = i;

#ifdef EVALUATOR_DEBUG
        printf("starting watch for %lu/%lu\n", (uintptr_t) e, i);
        fflush(stdout);
#endif
        err = ubik_env_watch(
                _load_callback, evaluator->env, node->load.loc, node_ref);
        return err;
}

no_ignore static ubik_error
run_state(
        struct ubik_evaluator *evaluator,
        size_t wid,
        struct ubik_eval_state *e)
{
        struct ubik_value *a;
        struct ubik_value *r;
        struct ubik_node *node;
        size_t i;
        size_t t;
        enum node_status old_status;
        enum node_status ts;
        ubik_error err;
        bool progress;

        pthread_mutex_lock(&e->lock);
        err = OK;

        if (e->f->fun.evaluator != NULL)
        {
                t = e->f->fun.result;
                err = e->f->fun.evaluator(
                        &e->nv[t], &e->nt[t], e->args, e->argtypes, e->f,
                        evaluator->ws);
                if (err != OK)
                        goto exit;
                e->term = 0;
                pthread_mutex_unlock(&e->lock);
                return OK;
        }

        progress = true;

        while (progress && e->term != 0)
        {
                progress = false;

                for (i = 0; i < e->n; i++)
                {
                        node = &e->f->fun.nodes[i];
                        old_status = status(e, i);
                        switch (old_status)
                        {
                        case DONE:
                        case DATA:
                        case APPLY:
                                continue;
                        case WAIT:
                                if (unlikely(node->is_terminal))
                                        setstatus(e, i, NEEDED);
                                break;
                        case LOADED:
                                setstatus(e, i, DONE);
                                goto postupdate;
                        case NEEDED:
                                break;
                        }

                        switch (node->node_type)
                        {
                        case UBIK_INPUT:
                                t = node->input.arg_num;
                                e->nv[i] = e->args[t];
                                e->nt[i] = e->argtypes[t];
                                setstatus(e, i, DONE);
                                break;

                        case UBIK_REF:
                                t = node->ref.referrent;
                                ts = status(e, t);
                                switch (ts)
                                {
                                case DONE:
                                case LOADED:
                                        e->nv[i] = e->nv[t];
                                        e->nt[i] = e->nt[t];
                                        setstatus(e, i, DONE);
                                        break;
                                case WAIT:
                                        if (status(e, i) == NEEDED)
                                                setstatus(e, t, NEEDED);
                                        break;
                                case DATA:
                                case APPLY:
                                case NEEDED:
                                        break;
                                }
                                break;

                        case UBIK_VALUE:
                                e->nv[i] = node->value.value;
                                e->nt[i] = node->value.type;
                                setstatus(e, i, DONE);
                                break;

                        case UBIK_COND:
                                run_cond(e, i, node);
                                break;

                        case UBIK_APPLY:
                                err = run_apply(evaluator, e, i, node);
                                if (err != OK)
                                        goto exit;
                                break;

                        case UBIK_LOAD:
                                err = run_load(evaluator, e, i, node);
                                if (err != OK)
                                        goto exit;
                                break;

                        case UBIK_STORE:
                                t = node->store.value;
                                ts = status(e, t);
                                switch (ts)
                                {
                                case DONE:
                                case LOADED:
                                        e->nv[i] = e->nv[t];
                                        e->nt[i] = e->nt[t];
                                        setstatus(e, i, DONE);
                                        break;
                                case WAIT:
                                        if (status(e, i) == NEEDED)
                                                setstatus(e, t, NEEDED);
                                        break;
                                case DATA:
                                case APPLY:
                                case NEEDED:
                                        break;
                                }

                                if (status(e, i) != DONE)
                                        break;
                                err = ubik_env_set(
                                        evaluator->env, node->store.loc,
                                        e->nv[i], e->nt[i]);
                                if (err != OK)
                                        goto exit;
                                break;

                        case UBIK_NATIVE:
                        case UBIK_MAX_NODE_TYPE:
                        default:
                                err = ubik_raise(
                                        ERR_BAD_TYPE, "unknown node type");
                                goto exit;
                        }

postupdate:
                        r = e->nv[i];
                        if (status(e, i) == DONE &&
                                (r->type == UBIK_PAP || r->type == UBIK_FUN))
                        {
                                t = 0;
                                a = r;
                                while (a->type == UBIK_PAP)
                                {
                                        a = a->pap.func;
                                        t++;
                                }
                                if (t == a->fun.arity)
                                {
                                        setstatus(e, i, APPLY);
                                        err = push(
                                                evaluator, r, e, i, wid, NULL);
                                        if (err != OK)
                                                goto exit;
                                }
                        }

                        if (node->is_terminal &&
                                        old_status != DONE &&
                                        status(e, i) == DONE)
                                e->term--;
                        if (e->term == 0)
                                break;
                        progress = progress || (old_status != status(e, i));
                }
        }

exit:
        pthread_mutex_unlock(&e->lock);
        return err;
}

no_ignore static ubik_error
push(
        struct ubik_evaluator *evaluator,
        struct ubik_value *v,
        struct ubik_eval_state *waiting,
        size_t wait_node,
        size_t worker_id,
        struct ubik_evaluate_callback *cb)
{
        struct ubik_value *a;
        struct ubik_eval_req *req;
        struct ubik_eval_state *e;
        size_t i;
        size_t arity;

        ubik_galloc((void **) &req, 1, sizeof(struct ubik_eval_req));
        ubik_galloc((void **) &e, 1, sizeof(struct ubik_eval_state));
        req->e = e;
        req->waiting = waiting;
        req->node = wait_node;
        req->cb = cb;

#ifdef EVALUATOR_DEBUG
        printf("pushing %lu notify %lu/%lu cb %lu\n",
                (uintptr_t) e, (uintptr_t) waiting, wait_node, (uintptr_t) cb);
        fflush(stdout);
#endif

        if (v->type == UBIK_FUN)
                e->f = v;
        else if (v->type == UBIK_PAP)
                e->f = v->pap.base_func;
        e->n = e->f->fun.n;
        arity = e->f->fun.arity;

        ubik_galloc((void **) &e->args, arity, sizeof(struct ubik_value *));
        ubik_galloc((void **) &e->argtypes, arity, sizeof(struct ubik_value *));
        for (i = arity, a = v; i > 0; i--, a = a->pap.func)
        {
                ubik_assert(a->type == UBIK_PAP);
                e->args[i - 1] = a->pap.arg;
                e->argtypes[i - 1] = a->pap.arg_type;
        }

        ubik_galloc((void **) &e->s, e->n, sizeof(enum node_status));
        ubik_galloc((void **) &e->nv, e->n, sizeof(struct ubik_value *));
        ubik_galloc((void **) &e->nt, e->n, sizeof(struct ubik_value *));

        e->term = 0;
        for (i = 0; i < e->n; i++)
                if (e->f->fun.nodes[i].is_terminal)
                        e->term++;

        memset(e->s, WAIT, e->n * sizeof(enum node_status));
        pthread_mutex_init(&e->lock, NULL);

        ubik_jobq_push(&evaluator->q, worker_id, req);
        return OK;
}

no_ignore ubik_error
ubik_evaluate_push(
        struct ubik_evaluator *evaluator,
        struct ubik_value *v,
        struct ubik_evaluate_callback *cb)
{
        return push(evaluator, v, NULL, 0, 0, cb);
}

no_ignore ubik_error
ubik_evaluate_push_roots(
        struct ubik_evaluator *evaluator,
        struct ubik_workspace *ws)
{
        ubik_error err;
        size_t i;
        size_t q;

        q = 0;
        for (; ws != NULL; ws = ws->next)
        {
                for (i = 0; i < ws->n; i++)
                {
                        if (!ws->values[i].gc.root)
                                continue;
                        err = push(
                                evaluator, &ws->values[i], NULL, 0,
                                q++ % evaluator->n_workers, NULL);
                        if (err != OK)
                                return err;
                }
        }

        return OK;
}

no_ignore static void *
run(void *e)
{
        struct ubik_evaluator *evaluator;
        struct ubik_eval_req *r;
        struct eval_thread_state *ets;
        ubik_error err;
        size_t t0;
        size_t t1;
        size_t wid;

        ets = (struct eval_thread_state *) e;
        evaluator = ets->e;
        wid = ets->id;
        err = OK;

        while (!evaluator->die)
        {
                r = ubik_jobq_pop(&evaluator->q, wid);
                if (r == NULL)
                        goto exit;

                err = run_state(evaluator, wid, r->e);
                if (err != OK)
                        goto exit;
                if (r->e->term != 0)
                {
                        ubik_jobq_push(&evaluator->q, wid, r);
                        continue;
                }

                t1 = r->e->f->fun.result;
#ifdef EVALUATOR_DEBUG
                printf("finished %lu notify %lu/%lu\n",
                        (uintptr_t) r->e, (uintptr_t) r->waiting, r->node);
                fflush(stdout);
#endif
                if (likely(r->waiting != NULL))
                {
                        pthread_mutex_lock(&r->waiting->lock);
                        t0 = r->node;
                        r->waiting->nv[t0] = r->e->nv[t1];
                        r->waiting->nt[t0] = r->e->nt[t1];
                        setstatus(r->waiting, t0, LOADED);
                        pthread_mutex_unlock(&r->waiting->lock);
                }
                if (unlikely(r->cb != NULL))
                {
                        pthread_mutex_lock(&r->e->lock);
                        err = r->cb->func(
                                r->cb, r->e->nv[t1], r->e->nt[t1], r->e->nv);
                        pthread_mutex_unlock(&r->e->lock);
                        if (err != OK)
                                goto exit;
                }

                free_eval_state(r->e);
                free(r->e);
                free(r);
        }

exit:
        if (err != OK)
                evaluator->die = true;
        return err;
}

static size_t
n_processors()
{
        long res;
        char *env;
        char *end;

        env = getenv("UBIK_PROCS");
        if (env != NULL)
        {
                res = strtol(env, &end, 10);
                if (end != env)
                        return res;
        }
#if defined(_SC_NPROCESSORS_ONLN)
        res = sysconf(_SC_NPROCESSORS_ONLN);
        if (res <= 0)
        {
                return 1;
        }
        return (size_t) res;
#else
        return 1;
#endif
}

no_ignore ubik_error
ubik_evaluate_run(struct ubik_evaluator *evaluator)
{
        size_t i;
        int res;
        ubik_error err;
        ubik_error worker_err;
        struct eval_thread_state *ets;
        pthread_t *workers;

        workers = NULL;

        if (evaluator->n_workers > 1)
        {
                ubik_galloc(
                        (void**) &workers, evaluator->n_workers - 1,
                        sizeof(pthread_t));

                for (i = 0; i < evaluator->n_workers - 1; i++)
                {
                        ubik_galloc(
                                (void**) &ets, 1,
                                sizeof(struct eval_thread_state));
                        ets->e = evaluator;
                        ets->id = i + 1;
                        res = pthread_create(&workers[i], NULL, run, ets);
                        ubik_assert(res == 0);
                }
        }

        ubik_galloc((void**) &ets, 1, sizeof(struct eval_thread_state));
        ets->e = evaluator;
        ets->id = 0;
        err = run(ets);

        if (evaluator->n_workers > 1)
        {
                for (i = 0; i < evaluator->n_workers - 1; i++)
                {
                        res = pthread_join(workers[i], (void **) &worker_err);
                        ubik_assert(res == 0);
                        if (err == OK && worker_err != OK)
                                err = worker_err;
                }
                free(workers);
        }

        return err;
}

no_ignore ubik_error
ubik_evaluate_new(
        struct ubik_evaluator **evaluator,
        struct ubik_env *env,
        struct ubik_workspace *ws)
{
        ubik_galloc((void **) evaluator, 1, sizeof(struct ubik_evaluator));
        (*evaluator)->env = env;
        (*evaluator)->ws = ws;
        (*evaluator)->die = false;
        (*evaluator)->n_workers = n_processors();
        ubik_jobq_init(&(*evaluator)->q, (*evaluator)->n_workers);
        return OK;
}

void
ubik_evaluate_free(struct ubik_evaluator *evaluator)
{
        ubik_jobq_free(&evaluator->q);
        free(evaluator);
}
