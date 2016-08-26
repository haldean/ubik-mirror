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

static char testprog[] =
        "~ testprog\n: test-resolve\n= \\x -> uadd x ((\\x -> x) x)\n";

test_t
resolve()
{
        struct ubik_compile_request req = {0};
        struct ubik_ast *ast;
        struct ubik_stream progstream;
        struct ubik_stream feedback;
        struct ubik_ast_expr *e;

        assert(ubik_stream_wfilep(&feedback, stdout) == OK);
        req.feedback = &feedback;

        assert(ubik_stream_buffer(&progstream, &req.region) == OK);
        /* drop the null byte off the end, it makes the lexer unhappy */
        assert(ubik_stream_write(
                &progstream, testprog, sizeof(testprog) - 1)
               == sizeof(testprog) - 1);
        assert(ubik_parse(
                &ast, &req.region, &feedback, "testprog", &progstream) == OK);
        assert(ubik_resolve(ast, &req) == OK);

        /* e here is \x -> uadd x ((\x -> x) x). The xs, by name, are
         * \x0 -> uadd x1 ((\x2 -> x3) x4) */
        e = ((struct ubik_ast_binding *) ast->bindings.elems[0])->expr;
        /* compare x1 and x4 */
        assert(e->lambda.body->apply.head->apply.tail->atom->name_loc->def ==
               e->lambda.body->apply.tail->apply.tail->atom->name_loc->def);
        /* make sure x1 and x3 are different */
        assert(e->lambda.body->apply.head->apply.tail->atom->name_loc->def !=
               e->lambda.body->apply.tail->apply.tail->atom->name_loc->def);

        ubik_alloc_free(&req.region);
        return ok;
}

run_single(resolve)
