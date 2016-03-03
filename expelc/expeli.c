/*
 * expeli.c: expel interpreter
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


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "expel/ast.h"
#include "expel/compile.h"
#include "expel/env.h"
#include "expel/expel.h"
#include "expel/parse.h"
#include "expel/schedule.h"
#include "expel/value.h"

#define c(x) \
        do { \
        err = x; \
        if (err != OK) \
        { \
                char *expl = xl_error_explain(err); \
                printf("%s\n", expl); \
                free(err); free(expl); \
                goto teardown; \
        } } while(0)

int
main(int argc, char *argv[])
{
        struct xl_ast *ast;
        struct xl_dagc **graphs;
        struct xl_stream sstdin;
        struct xl_env env;
        struct xl_scheduler *s;
        size_t n_graphs;
        xl_error err;

        c(xl_start());

        c(xl_stream_rfilep(&sstdin, stdin));

        c(xl_ast_new(&ast));

        c(xl_parse(ast, &sstdin));

        if (argc > 1 && strcmp(argv[1], "show-ast") == 0)
                c(xl_ast_print(ast));

        c(xl_compile_ast(&graphs, &n_graphs, ast, NULL));

        c(xl_env_init(&env));

        c(xl_schedule_new(&s));
        c(xl_schedule_push(s, graphs[0], &env, NULL));
        c(xl_schedule_run(s));

        c(xl_ast_free(ast));
        c(xl_env_free(&env));

teardown:
        err = xl_teardown();
        if (err != OK)
                printf("error when tearing down runtime\n");

        return err == OK ? EXIT_SUCCESS : EXIT_FAILURE;
}

