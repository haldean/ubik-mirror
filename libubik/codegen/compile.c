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
        char *debug_var;
        bool debug;
        ubik_error err;

        debug_var = getenv("UBIK_DEBUG");
        if (debug_var == NULL)
                debug = false;
        else
        {
                if (strcmp(debug_var, "0") == 0)
                        debug = false;
                else if (strcmp(debug_var, "no") == 0)
                        debug = false;
                else
                        debug = true;
        }
        cenv->debug = debug;

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
        struct ubik_compilation_result *res;
        struct ubik_compilation_request *req;
        size_t i, j;
        ubik_error err;

        free(cenv->scratch_dir);

        for (i = 0; i < cenv->n_include_dirs; i++)
                free(cenv->include_dirs[i]);
        free(cenv->include_dirs);

        for (i = 0; i < cenv->compiled.n; i++)
        {
                res = cenv->compiled.elems[i];

                free(res->request->source_name);
                free(res->request);

                err = ubik_ast_free(res->ast);
                if (err != OK)
                        return err;

                for (j = 0; j < res->n_graphs; j++)
                {
                        err = ubik_release(res->graphs[j]);
                        if (err != OK)
                                return err;
                }
                free(res->graphs);

                free(res);
        }
        ubik_vector_free(&cenv->compiled);

        for (i = 0; i < cenv->to_compile.n; i++)
        {
                req = cenv->to_compile.elems[i];
                free(req->source_name);
                free(req);
        }
        ubik_vector_free(&cenv->to_compile);

        return OK;
}

no_ignore static ubik_error
compile_request(
        struct ubik_compilation_env *cenv,
        struct ubik_compilation_job *job)
{
        struct ubik_ast *ast;
        struct ubik_gen_requires *requires;
        struct ubik_gen_requires *head;
        struct ubik_dagc *graph;
        struct ubik_compilation_result *res;
        local(resolve_context) struct ubik_resolve_context resolve_ctx = {0};
        local(patterns_context) struct ubik_patterns_context pattern_ctx = {0};
        ubik_error err;
        ubik_error free_err;

        err = ubik_parse(&ast, job->request->source_name, job->request->source);
        if (err != OK)
                return err;

        if (cenv->debug)
        {
                printf("parsed\n");
                err = ubik_ast_print(ast);
                if (err != OK)
                        goto free_ast;
        }

        err = ubik_patterns_compile_all(ast, &pattern_ctx);
        if (err != OK)
                return err;

        if (cenv->debug)
        {
                printf("\npatterns\n");
                err = ubik_ast_print(ast);
                if (err != OK)
                        goto free_ast;
        }

        err = ubik_resolve(ast, &resolve_ctx);
        if (err != OK)
                goto free_ast;
        if (cenv->debug)
        {
                printf("\nresolved\n");
                err = ubik_ast_print(ast);
                if (err != OK)
                        goto free_ast;
        }

        err = ubik_infer_types(ast, job->request->source);
        if (err != OK)
                goto free_ast;

        if (cenv->debug)
        {
                printf("\ninferred\n");
                err = ubik_ast_print(ast);
                if (err != OK)
                        goto free_ast;
        }

        requires = NULL;
        err = ubik_gen_graphs(&graph, &requires, ast, job->request->reason);
        if (err != OK)
                goto free_ast;

        head = requires;
        while (requires != NULL)
        {
                err = ubik_raise(
                        ERR_NOT_IMPLEMENTED, "imports not implemented");
                goto free_ast;
        }

        err = ubik_gen_requires_free(head);
        if (err != OK)
                goto free_ast;

        res = calloc(1, sizeof(struct ubik_compilation_result));
        if (res == NULL)
        {
                err = ubik_raise(ERR_NO_MEMORY, "compilation result alloc");
                goto free_ast;
        }
        res->request = job->request;
        res->ast = ast;
        res->graphs = calloc(1, sizeof(struct ubik_dagc *));
        if (res->graphs == NULL)
        {
                err = ubik_raise(ERR_NO_MEMORY, "compilation result alloc");
                goto free_res;
        }
        res->graphs[0] = graph;
        res->n_graphs = 1;

        err = ubik_vector_append(&cenv->compiled, res);
        if (err != OK)
                goto free_res_graphs;

        job->request->cb(res);
        free(job);
        return OK;

free_res_graphs:
        free(res->graphs);
free_res:
        free(res);
free_ast:
        free_err = ubik_ast_free(ast);
        if (err != OK)
                return err;
        if (free_err != OK)
                return free_err;
        return OK;
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
                        if (cenv->debug)
                                printf("found %s for %s\n",
                                       test_file, package_name);
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
ubik_compile_enqueue(
        struct ubik_compilation_env *cenv,
        struct ubik_compilation_request *userreq)
{
        struct ubik_compilation_request *req;
        struct ubik_compilation_job *job;

        req = calloc(1, sizeof(struct ubik_compilation_request));
        if (req == NULL)
                return ubik_raise(ERR_NO_MEMORY, "enqueue compilation request");

        job = calloc(1, sizeof(struct ubik_compilation_job));
        if (job == NULL)
        {
                free(req);
                return ubik_raise(ERR_NO_MEMORY, "enqueue compilation request");
        }

        req->source_name = strdup(userreq->source_name);
        req->source = userreq->source;
        req->reason = userreq->reason;
        req->cb = userreq->cb;

        job->request = req;
        job->ast = NULL;

        return ubik_vector_append(&cenv->to_compile, job);
}

no_ignore ubik_error
ubik_compile_run(struct ubik_compilation_env *cenv)
{
        struct ubik_compilation_job *job;
        ubik_error err;

        while (cenv->to_compile.n > 0)
        {
                job = cenv->to_compile.elems[0];
                err = compile_request(cenv, job);
                if (err != OK)
                        return err;
                cenv->to_compile.n--;
        }

        return OK;
}
