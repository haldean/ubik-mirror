/*
 * ubiki.c: ubik interpreter
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
#include "ubik/infer.h"
#include "ubik/parse.h"
#include "ubik/resolve.h"
#include "ubik/schedule.h"
#include "ubik/ubik.h"
#include "ubik/value.h"

#define c(x) \
        do { \
        err = x; \
        if (err != OK) \
        { \
                char *expl = ubik_error_explain(err); \
                printf("%s\n", expl); \
                free(err); free(expl); \
                goto teardown; \
        } } while(0)

int
main(int argc, char *argv[])
{
        struct ubik_ast *ast;
        struct ubik_dagc **graphs;
        struct ubik_stream sstdin;
        struct ubik_env env = {0};
        struct ubik_scheduler *s;
        local(resolve_context) struct ubik_resolve_context rctx = {0};
        size_t n_graphs;
        size_t i;
        ubik_error err;
        ubik_error teardown_err;
        ubik_error parse_err;
        char *buf;

        ast = NULL;
        graphs = NULL;
        s = NULL;

        c(ubik_start());

        c(ubik_stream_rfilep(&sstdin, stdin));

        parse_err = ubik_parse(&ast, "(stdin)", &sstdin);
        if (parse_err == OK)
                parse_err = ubik_resolve(ast, "(stdin)", &sstdin, &rctx);
        if (parse_err == OK)
                parse_err = ubik_infer_types(ast, "(stdin)", &sstdin);
        if (argc > 1 && strcmp(argv[1], "emit-ast") == 0)
        {
                if (ast != NULL)
                        err = ubik_ast_print(ast);
                else
                        printf("ast could not be parsed.\n");
        }
        c(parse_err);
        c(err);

        c(ubik_compile_ast(&graphs, &n_graphs, ast, LOAD_MAIN, NULL));

        c(ubik_env_init(&env));

        c(ubik_schedule_new(&s));
        c(ubik_schedule_push(s, graphs[0], &env, NULL));
        c(ubik_schedule_run(s));

teardown:
        if (ast != NULL)
        {
                teardown_err = ubik_ast_free(ast);
                if (teardown_err != OK)
                {
                        buf = ubik_error_explain(teardown_err);
                        printf("error when freeing ast: %s\n", buf);
                        free(buf);
                        free(teardown_err);
                }
        }

        if (s != NULL)
        {
                teardown_err = ubik_schedule_free(s);
                if (teardown_err != OK)
                {
                        buf = ubik_error_explain(teardown_err);
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
                        teardown_err = ubik_release(graphs[i]);
                        if (teardown_err != OK)
                        {
                                buf = ubik_error_explain(teardown_err);
                                printf("graph release failed: %s\n", buf);
                                free(buf);
                                free(teardown_err);
                        }
                }
                free(graphs);
        }

        teardown_err = ubik_env_free(&env);
        if (teardown_err != OK)
        {
                buf = ubik_error_explain(teardown_err);
                printf("error when freeing environment: %s\n", buf);
                free(buf);
                free(teardown_err);
        }

        teardown_err = ubik_teardown();
        if (teardown_err != OK)
        {
                buf = ubik_error_explain(teardown_err);
                printf("error when tearing down runtime: %s\n", buf);
                free(buf);
                free(teardown_err);
        }

        return err == OK ? EXIT_SUCCESS : EXIT_FAILURE;
}

