/*
 * compile.c: ubik compilation
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

#include "ubik/compile.h"
#include "ubik/infer.h"
#include "ubik/parse.h"
#include "ubik/patterns.h"
#include "ubik/resolve.h"
#include "ubik/string.h"
#include "ubik/util.h"


no_ignore ubik_error
ubik_compile_env_default(struct ubik_compilation_env *cenv)
{
        char *scratch_dir;
        char *include_dirs;
        ubik_error err;

        scratch_dir = calloc(512, sizeof(char));
        if (getcwd(scratch_dir, 500) == NULL)
        {
                perror("could not open current directory");
                return ubik_raise(ERR_UNEXPECTED_FAILURE, "getcwd");
        }
        strcat(scratch_dir, "/ubik-build");
        cenv->scratch_dir = scratch_dir;

        include_dirs = getenv("UBIK_INCLUDE");
        if (include_dirs == NULL)
        {
                cenv->include_dirs = NULL;
                cenv->n_include_dirs = 0;
        }
        else
        {
                err = ubik_string_split(
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

no_ignore ubik_error
ubik_compile_env_free(struct ubik_compilation_env *cenv)
{
        size_t i;

        free(cenv->scratch_dir);

        for (i = 0; i < cenv->n_include_dirs; i++)
                free(cenv->include_dirs[i]);
        free(cenv->include_dirs);

        return OK;
}

no_ignore ubik_error
ubik_compile_stream(
        struct ubik_dagc **res,
        char *source_name,
        struct ubik_stream *in_stream,
        char *uri_source,
        struct ubik_compilation_env *cenv,
        enum ubik_load_reason reason)
{
        struct ubik_ast *ast;
        local(resolve_context) struct ubik_resolve_context resolve_ctx = {0};
        local(patterns_context) struct ubik_patterns_context pattern_ctx = {0};
        ubik_error err;
        ubik_error free_err;

        err = ubik_parse(&ast, source_name, in_stream);
        if (err != OK)
                return err;

        printf("parsed\n");
        err = ubik_ast_print(ast);
        if (err != OK)
                goto free_ast;

        err = ubik_patterns_compile_all(ast, &pattern_ctx);
        if (err != OK)
                return err;

        printf("\npatterns\n");
        err = ubik_ast_print(ast);
        if (err != OK)
                goto free_ast;

        err = ubik_resolve(ast, &resolve_ctx);
        printf("\nresolved\n");
        free_err = ubik_ast_print(ast);
        if (err != OK || free_err != OK)
                goto free_ast;

        err = ubik_infer_types(ast, source_name, in_stream);
        if (err != OK)
                goto free_ast;

        printf("\ninferred\n");
        err = ubik_ast_print(ast);
        if (err != OK)
                goto free_ast;

        err = ubik_compile_ast(res, ast, reason, uri_source, cenv);
        if (err != OK)
                goto free_ast;

        printf("\ncompiled\n");
        err = ubik_ast_print(ast);
        if (err != OK)
                goto free_ast;

free_ast:
        free_err = ubik_ast_free(ast);
        if (err != OK)
                return err;
        if (free_err != OK)
                return free_err;
        return OK;
}

no_ignore ubik_error
ubik_compile(
        struct ubik_dagc **res,
        char *source_name,
        struct ubik_stream *in_stream,
        struct ubik_compilation_env *cenv)
{
        return ubik_compile_stream(
                res, source_name, in_stream, NULL, cenv, LOAD_MAIN);
}

no_ignore ubik_error
_open_stream_for_requirement(
        struct ubik_stream *out,
        char **source_name,
        char *package_name,
        struct ubik_compilation_env *cenv)
{
        size_t i;
        char *test_file;
        char *test_basename;
        ubik_error err;

        test_basename = calloc(strlen(package_name) + 4, sizeof(char));
        strcpy(test_basename, package_name);
        strcat(test_basename, ".uk");

        for (i = 0; i < cenv->n_include_dirs; i++)
        {
                err = ubik_string_path_concat(
                        &test_file, cenv->include_dirs[i], test_basename);
                if (err != OK)
                        return err;
                if (access(test_file, R_OK) == 0)
                {
                        err = ubik_stream_rfile(out, test_file);
                        if (err != OK)
                                return err;
#ifdef UBIK_COMPILE_DEBUG
                        printf("found %s for %s\n", test_file, package_name);
#endif
                        free(test_file);
                        *source_name = test_basename;
                        return OK;
                }
                free(test_file);
        }

        free(test_basename);
        return ubik_raise(ERR_ABSENT, package_name);
}

no_ignore ubik_error
ubik_compile_ast(
        struct ubik_dagc **res,
        struct ubik_ast *ast,
        enum ubik_load_reason reason,
        char *uri_source,
        struct ubik_compilation_env *cenv)
{
        struct ubik_gen_requires *requires;
        struct ubik_gen_requires *head;
        ubik_error err;

        unused(cenv);

        requires = NULL;
        err = ubik_gen_graphs(res, &requires, ast, reason, uri_source);
        if (err != OK)
                return err;

        head = requires;
        while (requires != NULL)
        {
                return ubik_raise(
                        ERR_NOT_IMPLEMENTED, "imports not implemented");
        }

        err = ubik_gen_requires_free(head);
        if (err != OK)
                return err;

        return OK;
}