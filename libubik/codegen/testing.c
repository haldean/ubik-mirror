/*
 * testing.c: tools for Ubik tests
 * Copyright (C) 2016, Haldean Brown
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

#include "ubik/assert.h"
#include "ubik/assign.h"
#include "ubik/ast.h"
#include "ubik/env.h"
#include "ubik/fun.h"
#include "ubik/resolve.h"
#include "ubik/schedule.h"
#include "ubik/string.h"
#include "ubik/testing.h"
#include "ubik/util.h"

static no_ignore ubik_error
compile_test(
        struct ubik_value **v,
        struct ubik_ast_test *test,
        struct ubik_stream *feedback,
        struct ubik_alloc_region *r,
        struct ubik_workspace *ws)
{
        /* Create a function that calls (eq actual expected) */
        struct ubik_ast_expr *e0, *e1, *eq;
        struct ubik_assign_context assign_ctx;
        struct ubik_vector nodes = {0};
        ubik_error err;

        assign_ctx.region = r;
        assign_ctx.feedback = feedback;
        assign_ctx.workspace = ws;
        assign_ctx.errors.region = r;
        nodes.region = r;

        ubik_alloc1(&eq, struct ubik_ast_expr, r);
        eq->expr_type = EXPR_ATOM;
        ubik_alloc1(&eq->atom, struct ubik_ast_atom, r);
        eq->atom->atom_type = ATOM_NAME;
        eq->atom->str = "eq";
        ubik_alloc1(&eq->atom->name_loc, struct ubik_resolve_name_loc, r);
        eq->atom->name_loc->type = RESOLVE_NATIVE;

        ubik_alloc1(&e0, struct ubik_ast_expr, r);
        e0->expr_type = EXPR_APPLY;
        e0->apply.head = eq;
        e0->apply.tail = test->actual;

        ubik_alloc1(&e1, struct ubik_ast_expr, r);
        e1->expr_type = EXPR_APPLY;
        e1->apply.head = e0;
        e1->apply.tail = test->expected;

        err = ubik_assign_nodes(&assign_ctx, &nodes, e1);
        if (err != OK)
                return err;

        err = ubik_value_new(v, ws);
        if (err != OK)
                return err;

        ubik_fun_from_vector(*v, &nodes, e1->gen);
        return OK;
}

struct test_callback_info
{
        struct ubik_ast_test *test;
        struct ubik_stream *feedback;
        bool *mark_failed;
};

static no_ignore ubik_error
test_callback(void *arg, struct ubik_scheduler *s, struct ubik_exec_unit *e)
{
        unused(s);
        struct test_callback_info *cb;
        struct ubik_value *res;

        res = e->gexec->nv[e->node];
        ubik_assert(res->type == UBIK_BOO);

        cb = (struct test_callback_info *) arg;
        ubik_fprintf(cb->feedback, res->boo.value ? "ok:   " : "fail: ");
        ubik_ast_expr_pretty(cb->feedback, cb->test->actual, 0);
        ubik_fprintf(cb->feedback, " == ");
        ubik_ast_expr_pretty(cb->feedback, cb->test->expected, 0);
        ubik_fprintf(cb->feedback, "\n");

        if (!res->boo.value)
                *cb->mark_failed = true;
        return OK;
}

no_ignore ubik_error
ubik_testing_run(struct ubik_compile_result *compiled)
{
        struct ubik_scheduler *sched;
        struct ubik_ast_test *test;
        struct ubik_value *v;
        struct ubik_env env;
        struct ubik_workspace *ws;
        struct ubik_exec_notify *notify;
        struct test_callback_info *tcb;
        bool any_failed;
        size_t i;
        ubik_error err;

        err = ubik_schedule_new(&sched);
        if (err != OK)
                return err;

        err = ubik_env_init(&env);
        if (err != OK)
                return err;

        err = ubik_schedule_push_roots(
                sched, &env, compiled->request->workspace);
        if (err != OK)
                return err;

        err = ubik_workspace_new(&ws);
        if (err != OK)
                return err;

        for (i = 0; i < compiled->ast->tests.n; i++)
        {
                test = (struct ubik_ast_test *) compiled->ast->tests.elems[i];

                err = compile_test(
                        &v, test, compiled->request->feedback,
                        &compiled->request->region, ws);
                if (err != OK)
                        return err;

                notify = calloc(1, sizeof(struct ubik_exec_notify));
                ubik_assert(notify != NULL);
                notify->notify = test_callback;
                ubik_alloc1(&tcb, struct test_callback_info,
                            &compiled->request->region);
                tcb->feedback = compiled->request->feedback;
                tcb->test = test;
                tcb->mark_failed = &any_failed;
                notify->arg = tcb;
                err = ubik_schedule_push(sched, v, &env, false, notify, ws);
                if (err != OK)
                        return err;
        }

        err = ubik_schedule_run(sched);
        if (err != OK)
                return err;

        err = ubik_schedule_free(sched);
        if (err != OK)
                return err;
        free(sched);

        err = ubik_env_free(&env);
        if (err != OK)
                return err;

        ubik_workspace_free(ws);
        if (err != OK)
                return err;

        if (any_failed)
                return ubik_raise(ERR_TEST_FAILED, "tests failed, aborting");
        return OK;
}

