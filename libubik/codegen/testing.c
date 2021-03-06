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
#include "ubik/evaluator.h"
#include "ubik/feedback.h"
#include "ubik/fun.h"
#include "ubik/resolve.h"
#include "ubik/string.h"
#include "ubik/testing.h"
#include "ubik/util.h"
#include "ubik/value.h"

#include <inttypes.h>
#include <stdatomic.h>

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
        /* must be first, so test_callback_info can be treated as an ecb */
        struct ubik_evaluate_callback ecb;
        struct ubik_ast_test *test;
        struct ubik_stream *feedback;
        atomic_uint_fast32_t *successes;
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
test_callback(
        struct ubik_evaluate_callback *ecb,
        struct ubik_value *res,
        struct ubik_value *restype,
        struct ubik_value **all_values)
{
        struct test_callback_info *cb;
        struct ubik_value *expected;
        struct ubik_value *actual;
        bool success;

        cb = (struct test_callback_info *) ecb;
        unused(restype);

        ubik_assert(res->type == UBIK_BOO);
        success = res->boo.value;
        expected = all_values[cb->test->expected->gen];
        actual = all_values[cb->test->actual->gen];

        if (!success)
        {
                ubik_feedback_header(
                        cb->feedback, UBIK_FEEDBACK_ERR, &cb->test->loc,
                        "test failed");

                ubik_fprintf(cb->feedback, "\t\x1b[32mactual:\x1b[0m   ");
                print_value(cb->feedback, actual);

                ubik_fprintf(cb->feedback, "\n\t\x1b[32mexpected:\x1b[0m ");
                print_value(cb->feedback, expected);

                ubik_fprintf(cb->feedback, "\n");
        }
        else
                (*cb->successes)++;
        return OK;
}

no_ignore ubik_error
ubik_testing_run(struct ubik_compile_result *compiled)
{
        struct ubik_evaluator *evaluator;
        struct ubik_ast_test *test;
        struct ubik_value *v;
        struct ubik_env env;
        struct ubik_workspace *ws;
        struct ubik_workspace *compile_ws;
        struct ubik_ast_loc loc = {0};
        struct test_callback_info *tcb;
        atomic_uint_fast32_t successes;
        size_t i;
        ubik_error err;

        if (compiled->ast->tests.n == 0)
                return OK;

        successes = 0;

        err = ubik_env_init(&env);
        if (err != OK)
                return err;

        err = ubik_workspace_new(&ws);
        if (err != OK)
                return err;

        err = ubik_evaluate_new(&evaluator, &env, ws);
        if (err != OK)
                return err;

        /* Push all modinits into the environment; this doesn't use push_roots,
         * because we don't want user initializers, just the env setters. */
        compile_ws = compiled->request->workspace;
        for (; compile_ws != NULL; compile_ws = compile_ws->next)
        {
                for (i = 0; i < compile_ws->n; i++)
                {
                        if (!compile_ws->values[i].gc.modinit)
                                continue;
                        err = ubik_evaluate_push(
                                evaluator, &compile_ws->values[i], NULL);
                        if (err != OK)
                                return err;
                }
        }

        for (i = 0; i < compiled->ast->tests.n; i++)
        {
                test = (struct ubik_ast_test *) compiled->ast->tests.elems[i];

                err = compile_test(
                        &v, test, compiled->request->feedback,
                        &compiled->request->region, ws);
                if (err != OK)
                        return err;

                ubik_alloc1(&tcb, struct test_callback_info,
                            &compiled->request->region);
                tcb->ecb.func = test_callback;
                tcb->feedback = compiled->request->feedback;
                tcb->test = test;
                tcb->successes = &successes;

                err = ubik_evaluate_push(evaluator, v, &tcb->ecb);
                if (err != OK)
                        return err;
        }

        err = ubik_evaluate_run(evaluator);
        if (err != OK)
                return err;

        ubik_evaluate_free(evaluator);

        err = ubik_env_free(&env);
        if (err != OK)
                return err;

        ubik_workspace_free(ws);
        if (err != OK)
                return err;

        if (successes != compiled->ast->tests.n)
                return ubik_raise(ERR_TEST_FAILED, "tests failed, aborting");

        loc.source_name = compiled->ast->loc.source_name;
        ubik_feedback_header(
                compiled->request->feedback, UBIK_FEEDBACK_SUCCESS, &loc,
                "%" PRIuFAST64 " tests passed", successes);
        return OK;
}

