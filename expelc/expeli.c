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

#include "ubik/ast.h"
#include "ubik/compile.h"
#include "ubik/env.h"
#include "ubik/ubik.h"
#include "ubik/parse.h"
#include "ubik/resolve.h"
#include "ubik/schedule.h"
#include "ubik/value.h"

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
        struct xl_env env = {0};
        struct xl_scheduler *s;
        local(resolve_context) struct xl_resolve_context rctx = {0};
        size_t n_graphs;
        size_t i;
        xl_error err;
        xl_error teardown_err;
        xl_error parse_err;
        char *buf;

        ast = NULL;
        graphs = NULL;
        s = NULL;

        c(xl_start());

        c(xl_stream_rfilep(&sstdin, stdin));

        parse_err = xl_parse(&ast, "(stdin)", &sstdin);
        if (parse_err == OK)
                parse_err = xl_resolve(ast, "(stdin)", &sstdin, &rctx);
        if (argc > 1 && strcmp(argv[1], "emit-ast") == 0)
                err = xl_ast_print(ast);
        c(parse_err);
        c(err);

        c(xl_compile_ast(&graphs, &n_graphs, ast, NULL));

        c(xl_env_init(&env));

        c(xl_schedule_new(&s));
        c(xl_schedule_push(s, graphs[0], &env, NULL));
        c(xl_schedule_run(s));

teardown:
        if (ast != NULL)
        {
                teardown_err = xl_ast_free(ast);
                if (teardown_err != OK)
                {
                        buf = xl_error_explain(teardown_err);
                        printf("error when freeing ast: %s\n", buf);
                        free(buf);
                        free(teardown_err);
                }
        }

        if (s != NULL)
        {
                teardown_err = xl_schedule_free(s);
                if (teardown_err != OK)
                {
                        buf = xl_error_explain(teardown_err);
                        printf("error when freeing scheduler: %s\n", buf);
                        free(buf);
                        free(teardown_err);
                }
                free(s);
        }

        if (graphs != NULL)
        {
                for (i = 0; i < n_graphs; i++)
                {
                        if (graphs[i] == NULL)
                                continue;
                        teardown_err = xl_release(graphs[i]);
                        if (teardown_err != OK)
                        {
                                buf = xl_error_explain(teardown_err);
                                printf("graph release failed: %s\n", buf);
                                free(buf);
                                free(teardown_err);
                        }
                }
                free(graphs);
        }

        teardown_err = xl_env_free(&env);
        if (teardown_err != OK)
        {
                buf = xl_error_explain(teardown_err);
                printf("error when freeing environment: %s\n", buf);
                free(buf);
                free(teardown_err);
        }

        teardown_err = xl_teardown();
        if (teardown_err != OK)
        {
                buf = xl_error_explain(teardown_err);
                printf("error when tearing down runtime: %s\n", buf);
                free(buf);
                free(teardown_err);
        }

        return err == OK ? EXIT_SUCCESS : EXIT_FAILURE;
}

