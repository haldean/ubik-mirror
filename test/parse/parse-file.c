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
#include "expel/parse.h"

#define CHECK_ERR(msg) \
        do { if (err != OK) \
        { \
                char *expl = xl_error_explain(err); \
                printf(msg ": %s\n", expl); \
                free(err); free(expl); \
                goto teardown; \
        } } while(0)

int
main()
{
        struct xl_stream s;
        struct xl_ast *ast;
        size_t i;
        xl_error err;

        err = xl_stream_rfilep(&s, stdin);
        CHECK_ERR("create file stream");

        err = xl_ast_new(&ast);
        CHECK_ERR("create ast");

        err = xl_parse(ast, &s);
        CHECK_ERR("parse");

        for (i = 0; i < ast->n_bindings; i++)
        {
                wprintf(L"bind %S\n", ast->bindings[i]->name);
        }

        err = xl_ast_free(ast);
        CHECK_ERR("free ast");

teardown:
        return EXIT_SUCCESS;
}

