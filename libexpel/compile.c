/*
 * compile.c: expel compilation
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

#include "expel/compile.h"
#include "expel/gen.h"
#include "expel/parse.h"
#include "expel/util.h"

no_ignore xl_error
xl_compile(
        struct xl_dagc ***graphs,
        size_t *n_graphs,
        struct xl_stream *in_stream,
        char *scratch_dir)
{
        struct xl_ast *ast;
        xl_error err;

        err = xl_ast_new(&ast);
        if (err != OK)
                return err;

        err = xl_parse(ast, in_stream);
        if (err != OK)
                return err;

        err = xl_compile_ast(graphs, n_graphs, ast, scratch_dir);
        if (err != OK)
                return err;

        err = xl_ast_free(ast);
        if (err != OK)
                return err;

        return OK;
}

no_ignore xl_error
xl_compile_ast(
        struct xl_dagc ***graphs,
        size_t *n_graphs,
        struct xl_ast *ast,
        char *scratch_dir)
{
        struct xl_gen_requires *requires;
        xl_error err;

        unused(scratch_dir);

        requires = NULL;
        err = xl_compile_unit(
                graphs, n_graphs, &requires, ast, LOAD_MAIN);
        if (err != OK)
                return err;

        if (requires != NULL)
        {
                while (requires != NULL)
                {
                        printf("requires %s\n", requires->dependency->source);
                        requires = requires->next;
                }
                return xl_raise(ERR_NOT_IMPLEMENTED, "imports not implemented");
        }
        return OK;
}
