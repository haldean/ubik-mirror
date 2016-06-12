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

#include <dirent.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "ubik/compile.h"
#include "ubik/import.h"
#include "ubik/infer.h"
#include "ubik/parse.h"
#include "ubik/patterns.h"
#include "ubik/resolve.h"
#include "ubik/string.h"
#include "ubik/util.h"

no_ignore ubik_error
ubik_compile_env_default(struct ubik_compile_env *cenv)
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

static void
free_req(struct ubik_compile_request *req)
{
        free(req->source_name);
        if (req->package_name != NULL)
                free(req->package_name);
        free(req);
}

no_ignore ubik_error
ubik_compile_env_free(struct ubik_compile_env *cenv)
{
        struct ubik_compile_result *res;
        struct ubik_compile_job *job;
        size_t i, j;
        ubik_error err;

        free(cenv->scratch_dir);

        for (i = 0; i < cenv->n_include_dirs; i++)
                free(cenv->include_dirs[i]);
        free(cenv->include_dirs);

        for (i = 0; i < cenv->compiled.n; i++)
        {
                res = cenv->compiled.elems[i];

                free_req(res->request);

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
                job = cenv->to_compile.elems[i];
                free_req(job->request);
                if (job->ast != NULL)
                {
                        err = ubik_ast_free(job->ast);
                        if (err != OK)
                                return err;
                }
                ubik_vector_free(&job->dep_graphs);
                free(job);
        }
        ubik_vector_free(&cenv->to_compile);

        return OK;
}

no_ignore static ubik_error
create_import_request(
        struct ubik_compile_request *res,
        struct ubik_compile_env *cenv,
        char *name)
{
        struct dirent *test_f;
        struct ubik_stream in_stream;
        struct ubik_ast *test_ast;
        DIR *test_dir;
        char *fullpath;
        size_t i;
        ubik_error err;

        err = OK;

        for (i = 0; i < cenv->n_include_dirs; i++)
        {
                test_dir = opendir(cenv->include_dirs[i]);
                if (test_dir == NULL)
                {
                        perror("couldn't open include directory");
                        err = ubik_raise(ERR_SYSTEM, "open include directory");
                        goto free_fullpath;
                }

                while ((test_f = readdir(test_dir)) != NULL)
                {
                        if (!ubik_string_endswith(test_f->d_name, ".uk"))
                                continue;

                        err = ubik_string_path_concat(
                                &fullpath, cenv->include_dirs[i],
                                test_f->d_name);
                        if (err != OK)
                                goto free_fullpath;

                        err = ubik_stream_rfile(&in_stream, fullpath);
                        if (err != OK)
                                goto free_fullpath;

                        err = ubik_parse(&test_ast, fullpath, &in_stream, true);
                        if (err != OK)
                        {
                                free(fullpath);
                                continue;
                        }

                        if (strcmp(test_ast->package_name, name) == 0)
                        {
                                err = ubik_ast_free(test_ast);
                                if (err != OK)
                                        goto free_fullpath;

                                ubik_stream_reset(&in_stream);
                                res->source_name = fullpath;
                                res->package_name = strdup(name);
                                res->source = in_stream;
                                res->reason = LOAD_IMPORTED;
                                res->cb = NULL;

                                closedir(test_dir);
                                return OK;
                        }

                        err = ubik_ast_free(test_ast);
                        if (err != OK)
                                goto free_fullpath;

                        free(fullpath);
                }

                closedir(test_dir);
        }

        return ubik_raise(ERR_ABSENT, "couldn't find import source");

free_fullpath:
        free(fullpath);
        return err;
}

no_ignore static ubik_error
load_ast(struct ubik_compile_env *cenv, struct ubik_compile_job *job)
{
        ubik_error err;
        struct ubik_ast_import_list *import;
        struct ubik_compile_request *req;

        err = ubik_parse(
                &job->ast, job->request->source_name, &job->request->source,
                true);
        if (err != OK)
                return err;

        import = job->ast->imports;
        while (import != NULL)
        {
                req = calloc(1, sizeof(struct ubik_compile_request));
                err = create_import_request(req, cenv, import->canonical);
                if (err != OK)
                        return err;
                err = ubik_compile_enqueue(cenv, req);
                if (err != OK)
                {
                        free_req(req);
                        return err;
                }
                free_req(req);
                import = import->next;
        }

        job->status = job->ast->imports == NULL
                ? COMPILE_READY
                : COMPILE_WAIT_FOR_IMPORTS;
        return OK;
}

no_ignore static ubik_error
compile_job(
        struct ubik_compile_env *cenv,
        struct ubik_compile_job *job)
{
        struct ubik_dagc *graph;
        struct ubik_compile_result *res;
        local(resolve_context) struct ubik_resolve_context resolve_ctx = {0};
        local(patterns_context) struct ubik_patterns_context pattern_ctx = {0};
        size_t i;
        ubik_error err;

        err = ubik_patterns_compile_all(job->ast, &pattern_ctx);
        if (err != OK)
                return err;

        if (cenv->debug)
        {
                printf("\npatterns\n");
                err = ubik_ast_print(job->ast);
                if (err != OK)
                        return err;
        }

        err = ubik_import_add_splats(cenv, job->ast);
        if (err != OK)
                return err;
        if (cenv->debug)
        {
                printf("\nsplats\n");
                err = ubik_ast_print(job->ast);
                if (err != OK)
                        return err;
        }

        err = ubik_resolve(job->ast, &resolve_ctx);
        if (err != OK)
                return err;
        if (cenv->debug)
        {
                printf("\nresolved\n");
                err = ubik_ast_print(job->ast);
                if (err != OK)
                        return err;
        }

        err = ubik_infer_types(job->ast, &job->request->source);
        if (err != OK)
                return err;

        err = ubik_gen_graphs(
                &graph, job->ast, job->request->reason);
        if (err != OK)
                return err;

        res = calloc(1, sizeof(struct ubik_compile_result));
        if (res == NULL)
                return ubik_raise(ERR_NO_MEMORY, "compilation result alloc");
        res->request = job->request;
        res->ast = job->ast;
        res->graphs = calloc(1 + job->dep_graphs.n, sizeof(struct ubik_dagc *));
        if (res->graphs == NULL)
        {
                err = ubik_raise(ERR_NO_MEMORY, "compilation result alloc");
                goto free_res;
        }
        res->graphs[0] = graph;
        for (i = 0; i < job->dep_graphs.n; i++)
                res->graphs[i + 1] = job->dep_graphs.elems[i];
        res->n_graphs = 1 + job->dep_graphs.n;

        err = ubik_vector_append(&cenv->compiled, res);
        if (err != OK)
                goto free_res_graphs;

        if (job->request->cb != NULL)
                job->request->cb(res);
        job->status = COMPILE_DONE;
        return OK;

free_res_graphs:
        free(res->graphs);
free_res:
        free(res);
        return err;
}

no_ignore ubik_error
ubik_compile_enqueue(
        struct ubik_compile_env *cenv,
        struct ubik_compile_request *userreq)
{
        struct ubik_compile_request *req;
        struct ubik_compile_job *job;

        req = calloc(1, sizeof(struct ubik_compile_request));
        if (req == NULL)
                return ubik_raise(ERR_NO_MEMORY, "enqueue compilation request");

        job = calloc(1, sizeof(struct ubik_compile_job));
        if (job == NULL)
        {
                free(req);
                return ubik_raise(ERR_NO_MEMORY, "enqueue compilation request");
        }

        req->source_name = strdup(userreq->source_name);
        if (userreq->package_name != NULL)
                req->package_name = strdup(userreq->package_name);
        else
                req->package_name = NULL;
        req->source = userreq->source;
        req->reason = userreq->reason;
        req->cb = userreq->cb;

        job->request = req;
        job->ast = NULL;
        job->status = COMPILE_WAIT_FOR_AST;

        return ubik_vector_append(&cenv->to_compile, job);
}

no_ignore static ubik_error
ensure_imports_ready(
        struct ubik_compile_env *cenv,
        struct ubik_compile_job *job)
{
        struct ubik_ast_import_list *import;
        struct ubik_compile_result *result;
        bool found;
        char *expect;
        char *check;
        size_t i;
        ubik_error err;

        import = job->ast->imports;

        while (import != NULL)
        {
                expect = import->canonical;
                for (i = 0; i < cenv->compiled.n; i++)
                {
                        result = cenv->compiled.elems[i];
                        check = result->request->package_name;
                        if (check == NULL)
                                continue;
                        if (strcmp(check, expect) == 0)
                        {
                                found = true;
                                break;
                        }
                }
                if (!found)
                        return ubik_raise(
                                ERR_UNEXPECTED_FAILURE,
                                "imports were not satisfied in compilation");
                for (i = 0; i < result->n_graphs; i++)
                {
                        err = ubik_vector_append(
                                &job->dep_graphs, result->graphs[i]);
                        if (err != OK)
                                return err;
                        err = ubik_take(result->graphs[i]);
                        if (err != OK)
                                return err;
                }
                import = import->next;
        }

        job->status = COMPILE_READY;
        return OK;
}

no_ignore ubik_error
ubik_compile_run(struct ubik_compile_env *cenv)
{
        struct ubik_compile_job *job;
        ubik_error err;

        while (cenv->to_compile.n > 0)
        {
                job = cenv->to_compile.elems[cenv->to_compile.n - 1];

                switch (job->status)
                {
                case COMPILE_WAIT_FOR_AST:
                        err = load_ast(cenv, job);
                        if (err != OK)
                                return err;
                        break;

                case COMPILE_WAIT_FOR_IMPORTS:
                        err = ensure_imports_ready(cenv, job);
                        if (err != OK)
                                return err;
                        break;

                case COMPILE_READY:
                        err = compile_job(cenv, job);
                        if (err != OK)
                                return err;
                        break;

                case COMPILE_DONE:
                        ubik_vector_free(&job->dep_graphs);
                        free(job);
                        cenv->to_compile.n--;
                        break;
                }
        }

        return OK;
}
