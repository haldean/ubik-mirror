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
#include "ubik/feedback.h"
#include "ubik/fun.h"
#include "ubik/resolve.h"
#include "ubik/schedule.h"
#include "ubik/string.h"
#include "ubik/testing.h"
#include "ubik/util.h"
#include "ubik/value.h"

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

static void
print_value(struct ubik_stream *s, struct ubik_value *v)
{
        char *buf;
        size_t buflen;
        ubik_error err;

        err = ubik_value_humanize(&buf, &buflen, v);
        if (err != OK)
        {
                buf = ubik_error_explain(err);
                ubik_fprintf(s, "(unable to print value: %s)", buf);
                free(buf);
                return;
        }
        ubik_fprintf(s, "\"");
        ubik_assert(ubik_stream_write(s, buf, buflen) == buflen);
        ubik_fprintf(s, "\"");
        free(buf);
}

static no_ignore ubik_error
test_callback(void *arg, struct ubik_scheduler *s, struct ubik_exec_unit *e)
{
        unused(s);
        struct test_callback_info *cb;
        struct ubik_value *res;
        struct ubik_value *expected;
        struct ubik_value *actual;
        bool success;

        cb = (struct test_callback_info *) arg;

        res = e->gexec->nv[e->node];
        ubik_assert(res->type == UBIK_BOO);
        success = res->boo.value;
        expected = e->gexec->nv[cb->test->expected->gen];
        actual = e->gexec->nv[cb->test->actual->gen];

        if (success)
        {
                ubik_feedback_header(
                        cb->feedback, UBIK_FEEDBACK_SUCCESS, &cb->test->loc,
                        "test passed");
        }
        else
        {
                ubik_feedback_header(
                        cb->feedback, UBIK_FEEDBACK_ERR, &cb->test->loc,
                        "test failed");

                ubik_fprintf(cb->feedback, "\t\x1b[32mactual:\x1b[0m   ");
                print_value(cb->feedback, actual);

                ubik_fprintf(cb->feedback, "\n\t\x1b[32mexpected:\x1b[0m ");
                print_value(cb->feedback, expected);

                ubik_fprintf(cb->feedback, "\n");
                *cb->mark_failed = true;
        }
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

        any_failed = false;

        err = ubik_schedule_new(&sched);
        if (err != OK)
                return err;

        err = ubik_env_init(&env);
        if (err != OK)
                return err;

        /* Push all modinits into the environment; this doesn't use push_roots,
         * because we don't want user initializers, just the env setters. */
        ws = compiled->request->workspace;
        for (; ws != NULL; ws = ws->next)
        {
                for (i = 0; i < ws->n; i++)
                {
                        if (!ws->values[i].gc.modinit)
                                continue;
                        err = ubik_schedule_push(
                                sched, &ws->values[i], &env, false, NULL, ws);
                        if (err != OK)
                                return err;
                }
        }

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

