/*
 * resolve.c: run tests on name resolution
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

#include "ubik/compile.h"
#include "ubik/parse.h"
#include "ubik/resolve.h"
#include "unit.h"
#include <stdio.h>

static char testprog1[] =
        "~ testprog\n: test-resolve\n= \\x -> uadd x ((\\x -> x) x)\n";

test_t
resolve()
{
        struct ubik_compile_request req = {0};
        struct ubik_ast *ast;
        struct ubik_stream progstream;
        struct ubik_stream feedback;
        struct ubik_ast_expr *e;
        struct ubik_resolve_name_loc *x0, *x1, *x2, *x3, *x4;

        assert(ubik_stream_wfilep(&feedback, stdout) == OK);
        req.feedback = &feedback;

        assert(ubik_stream_buffer(&progstream, &req.region) == OK);
        /* drop the null byte off the end, it makes the lexer unhappy */
        assert(ubik_stream_write(
                &progstream, testprog1, sizeof(testprog1) - 1)
               == sizeof(testprog1) - 1);
        assert(ubik_parse(
                &ast, &req.region, &feedback, "testprog1", &progstream) == OK);
        assert(ubik_resolve(ast, &req) == OK);

        /* e here is \x -> uadd x ((\x -> x) x). The xs, by name, are
         * \x0 -> uadd x1 ((\x2 -> x3) x4) */
        e = ((struct ubik_ast_binding *) ast->bindings.elems[0])->expr;

        x0 = e->lambda.args->name_loc;
        x1 = e->lambda.body->apply.head->apply.tail->atom->name_loc;
        x2 = e->lambda.body->apply.tail->apply.head->lambda.args->name_loc;
        x3 = e->lambda.body->apply.tail->apply.head->lambda.body->atom->name_loc;
        x4 = e->lambda.body->apply.tail->apply.tail->atom->name_loc;

        assert(x0->def == x1->def);
        assert(x0->def != x2->def);
        assert(x0->def != x3->def);
        assert(x0->def == x4->def);
        assert(x2->def == x3->def);

        ubik_alloc_free(&req.region);
        return ok;
}

static char testprog2[] = "~ t : b ^ Word = { : x = \\y -> y ! \\z -> x 8 }";

test_t
closure_regression()
{
        struct ubik_compile_request req = {0};
        struct ubik_ast *ast;
        struct ubik_stream progstream;
        struct ubik_stream feedback;

        assert(ubik_stream_wfilep(&feedback, stdout) == OK);
        req.feedback = &feedback;

        assert(ubik_stream_buffer(&progstream, &req.region) == OK);
        /* drop the null byte off the end, it makes the lexer unhappy */
        assert(ubik_stream_write(
                       &progstream, testprog2, sizeof(testprog2) - 1)
               == sizeof(testprog2) - 1);
        assert(ubik_parse(
                &ast, &req.region, &feedback, "testprog2", &progstream) == OK);
        assert(ubik_resolve(ast, &req) == OK);

        ubik_alloc_free(&req.region);
        return ok;
}

static char testprog3[] =
        "~ t : b ^ Word = ? x {"
        "  . T1 a => a"
        "  . T2 a b => b"
        "}";

test_t
pattern_define()
{
        struct ubik_compile_request req = {0};
        struct ubik_ast *ast;
        struct ubik_stream progstream;
        struct ubik_stream feedback;
        struct ubik_resolve_name_loc *x0, *x1;
        struct ubik_ast_expr *e;

        assert(ubik_stream_wfilep(&feedback, stdout) == OK);
        req.feedback = &feedback;

        assert(ubik_stream_buffer(&progstream, &req.region) == OK);
        /* drop the null byte off the end, it makes the lexer unhappy */
        assert(ubik_stream_write(
                       &progstream, testprog3, sizeof(testprog3) - 1)
               == sizeof(testprog3) - 1);
        assert(ubik_parse(
                       &ast, &req.region, &feedback, "testprog3", &progstream) == OK);
        assert(ubik_resolve(ast, &req) == OK);

        e = ((struct ubik_ast_binding *) ast->bindings.elems[0])->expr;
        x0 = e->cond_block.case_stmts->head->apply.tail->atom->name_loc;
        x1 = e->cond_block.case_stmts->tail->atom->name_loc;

        assert(x0->def == x1->def);

        ubik_alloc_free(&req.region);
        return ok;
}

int
main()
{
        init();
        run(resolve);
        run(closure_regression);
        finish();
}
