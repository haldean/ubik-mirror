/*
 * emit-tokens.c: tokenizer test
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
#include <wchar.h>

#include "expel/ast.h"
#include "expel/env.h"
#include "expel/expel.h"
#include "expel/gen.h"
#include "expel/parse.h"
#include "expel/schedule.h"
#include "expel/value.h"

#define c(x) \
        do { \
        err = x; \
        if (err != OK) \
        { \
                char *expl = xl_error_explain(err); \
                printf(#x ": %s\n", expl); \
                free(err); free(expl); \
                goto teardown; \
        } } while(0)

int
main()
{
        struct xl_ast *ast;
        struct xl_dagc **graphs;
        struct xl_stream sstdin;
        struct xl_stream sstdout;
        struct xl_env env;
        struct xl_scheduler *s;
        struct xl_value *actual;
        size_t n_graphs;
        xl_error err;

        c(xl_stream_rfilep(&sstdin, stdin));
        c(xl_stream_wfilep(&sstdout, stdout));

        c(xl_ast_new(&ast));

        c(xl_parse(ast, &sstdin));
        c(xl_ast_print(ast));

        c(xl_compile(&graphs, &n_graphs, ast));

        c(xl_env_init(&env));

        c(xl_schedule_new(&s));
        c(xl_schedule_push(s, graphs[0], &env, NULL));
        c(xl_schedule_run(s));

        actual = graphs[0]->result->known.tree;
        if (actual != NULL)
                c(xl_value_print(&sstdout, actual));
        else
                wprintf(L"no result calculated\n");

        c(xl_ast_free(ast));
        c(xl_env_free(&env));

teardown:
        return EXIT_SUCCESS;
}

