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
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "expel/compile.h"
#include "expel/gen.h"
#include "expel/parse.h"
#include "expel/string.h"
#include "expel/util.h"


no_ignore xl_error
xl_compile_default_env(struct xl_compilation_env *cenv)
{
        char *scratch_dir;
        char *include_dirs;
        xl_error err;

        scratch_dir = calloc(512, sizeof(char));
        if (getcwd(scratch_dir, 500) == NULL)
        {
                perror("could not open current directory");
                return xl_raise(ERR_UNEXPECTED_FAILURE, "getcwd");
        }
        strcat(scratch_dir, "/expel-build");
        cenv->scratch_dir = scratch_dir;

        include_dirs = getenv("EXPEL_INCLUDE");
        if (include_dirs == NULL)
        {
                cenv->include_dirs = NULL;
                cenv->n_include_dirs = 0;
        }
        else
        {
                err = xl_string_split(
                        &cenv->include_dirs,
                        &cenv->n_include_dirs,
                        include_dirs,
                        strlen(include_dirs),
                        ':');
                if (err != OK)
                        return err;
        }

        return OK;
}

no_ignore xl_error
xl_compile(
        struct xl_dagc ***graphs,
        size_t *n_graphs,
        struct xl_stream *in_stream,
        struct xl_compilation_env *cenv)
{
        struct xl_ast *ast;
        xl_error err;

        err = xl_ast_new(&ast);
        if (err != OK)
                return err;

        err = xl_parse(ast, in_stream);
        if (err != OK)
                return err;

        err = xl_compile_ast(graphs, n_graphs, ast, cenv);
        if (err != OK)
                return err;

        err = xl_ast_free(ast);
        if (err != OK)
                return err;

        return OK;
}

no_ignore static xl_error
_open_stream_for_requirement(
        char *package_name,
        struct xl_compilation_env *cenv)
{
        unused(package_name);
        unused(cenv);
        return OK;
}

no_ignore xl_error
xl_compile_ast(
        struct xl_dagc ***graphs,
        size_t *n_graphs,
        struct xl_ast *ast,
        struct xl_compilation_env *cenv)
{
        struct xl_gen_requires *requires;
        xl_error err;

        unused(cenv);

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
                        err = _open_stream_for_requirement(
                                requires->dependency->source, cenv);
                        if (err != OK)
                                return err;
                        requires = requires->next;
                }
                return xl_raise(ERR_NOT_IMPLEMENTED, "imports not implemented");
        }
        return OK;
}
