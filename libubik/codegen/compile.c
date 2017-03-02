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

#include "ubik/adt.h"
#include "ubik/assert.h"
#include "ubik/assign.h"
#include "ubik/compile.h"
#include "ubik/feedback.h"
#include "ubik/gen.h"
#include "ubik/import.h"
#include "ubik/infer.h"
#include "ubik/interfaces.h"
#include "ubik/literate.h"
#include "ubik/hooks.h"
#include "ubik/parse.h"
#include "ubik/patterns.h"
#include "ubik/resolve.h"
#include "ubik/string.h"
#include "ubik/testing.h"
#include "ubik/typesystem.h"
#include "ubik/util.h"

#define dprint(...) do { if (cenv->debug) printf(__VA_ARGS__); } while (0)

no_ignore ubik_error
ubik_compile_env_default(
        struct ubik_compile_env *cenv,
        struct ubik_workspace *ws)
{
        char *scratch_dir;
        char *include_dirs;
        char *debug_var;
        bool debug;
        ubik_error err;

        ubik_alloc_start(&cenv->region);

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

        ubik_ralloc((void **) &scratch_dir, 512, sizeof(char), &cenv->region);
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
                        ':',
                        &cenv->region);
                if (err != OK)
                        return err;
        }

        cenv->to_compile.region = &cenv->region;
        cenv->compiled.region = &cenv->region;

        err = ubik_typesystem_init(&cenv->type_system, &cenv->region, ws);
        if (err != OK)
                return err;

        return OK;
}

static void
free_req(struct ubik_compile_request *req)
{
        ubik_alloc_free(&req->region);
        free(req);
}

/* Frees the parts of a job object that are uniquely owned by the job. This
 * doesn't free the request or the AST, as those are present in the request
 * holder as well. */
no_ignore ubik_error
ubik_compile_env_free(struct ubik_compile_env *cenv)
{
        struct ubik_compile_result *res;
        struct ubik_compile_job *job;
        size_t i;

        for (i = 0; i < cenv->compiled.n; i++)
        {
                res = cenv->compiled.elems[i];
                free_req(res->request);
        }

        for (i = 0; i < cenv->to_compile.n; i++)
        {
                job = cenv->to_compile.elems[i];
                free_req(job->request);
        }

        ubik_alloc_free(&cenv->region);

        return OK;
}

no_ignore static ubik_error
create_import_request(
        struct ubik_compile_request *res,
        struct ubik_compile_env *cenv,
        char *name,
        struct ubik_compile_request *parent,
        struct ubik_ast_loc *loc)
{
        struct dirent *test_f;
        struct ubik_stream in_stream;
        struct ubik_ast *test_ast;
        struct ubik_ast_loc err_loc = {0};
        DIR *test_dir;
        char *fullpath;
        size_t i;
        ubik_error err;

        err = OK;

        dprint("searching for source for \"%s\"\n", name);

        for (i = 0; i < cenv->n_include_dirs; i++)
        {
                dprint("\tchecking directory \"%s\"\n", cenv->include_dirs[i]);
                test_dir = opendir(cenv->include_dirs[i]);
                if (test_dir == NULL)
                {
                        perror("couldn't open include directory");
                        return ubik_raise(ERR_SYSTEM, "open include directory");
                }

                while ((test_f = readdir(test_dir)) != NULL)
                {
                        ubik_local_region(r);
                        dprint("\tchecking \"%s\"\n", test_f->d_name);

                        if (!ubik_string_endswith(test_f->d_name, ".uk"))
                                continue;

                        err = ubik_string_path_concat(
                                &fullpath, cenv->include_dirs[i],
                                test_f->d_name, &r);
                        if (err != OK)
                                return err;

                        err = ubik_stream_rfile(&in_stream, fullpath);
                        if (err != OK)
                                return err;

                        err = ubik_parse(
                                &test_ast, &r, NULL, fullpath, &in_stream);
                        if (err != OK)
                        {
                                err_loc.source_name = fullpath;
                                ubik_feedback_header(
                                        parent->feedback,
                                        UBIK_FEEDBACK_WARN,
                                        &err_loc,
                                        "skipping potential import, failed to "
                                        "parse file");
                                free(err);
                                continue;
                        }

                        if (strcmp(test_ast->package_name, name) == 0)
                        {
                                dprint("found source for %s at %s\n",
                                       name, test_f->d_name);
                                ubik_stream_reset(&in_stream);
                                ubik_alloc_start(&res->region);
                                res->source_name = ubik_strdup(
                                        fullpath, &res->region);
                                res->package_name = ubik_strdup(
                                        name, &res->region);
                                res->source = in_stream;
                                res->reason = LOAD_IMPORTED;
                                res->cb = NULL;
                                res->feedback = parent->feedback;
                                res->workspace = parent->workspace;

                                closedir(test_dir);
                                return OK;
                        }
                }

                closedir(test_dir);
        }

        ubik_feedback_line(
                parent->feedback, UBIK_FEEDBACK_ERR, loc,
                "could not find source for imported package %s", name);
        return ubik_raise(ERR_ABSENT, "couldn't find import source");
}

no_ignore static ubik_error
load_ast(struct ubik_compile_env *cenv, struct ubik_compile_job *job)
{
        ubik_error err;
        struct ubik_ast_import_list *import;
        struct ubik_compile_request *req;

        err = ubik_literate_weave(
                &job->woven, &job->request->source, job->request->source_name,
                &job->request->region);
        if (err != OK)
                return err;

        err = ubik_parse(
                &job->ast, &job->request->region, job->request->feedback,
                job->request->source_name, &job->woven);
        if (err != OK)
                return err;

        if (job->request->package_name == NULL)
                job->request->package_name = job->ast->package_name;

        import = job->ast->imports;
        while (import != NULL)
        {
                req = calloc(1, sizeof(struct ubik_compile_request));
                err = create_import_request(
                        req, cenv, import->canonical, job->request,
                        &import->loc);
                if (err != OK)
                {
                        free(req);
                        return err;
                }
                err = ubik_compile_enqueue(cenv, req);
                free_req(req);
                if (err != OK)
                        return err;
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
        struct ubik_compile_result *res;
        local(infer_context) struct ubik_infer_context infer_ctx = {0};
        ubik_error err;

        dprint("compiling package %s\n", job->ast->package_name);

        err = ubik_import_add_all(cenv, job->ast, &job->request->region);
        if (err != OK)
                return err;
        if (cenv->debug)
        {
                printf("\nsplats\n");
                err = ubik_ast_print(job->ast);
                if (err != OK)
                        return err;
        }

        err = ubik_typesystem_load(
                cenv->type_system, job->ast, job->request);
        if (err != OK)
                return err;
        if (cenv->debug)
                ubik_typesystem_dump(cenv->type_system);

        err = ubik_resolve(job->ast, job->request);
        if (err != OK)
                return err;
        if (cenv->debug)
        {
                printf("\npre-resolve\n");
                err = ubik_ast_print(job->ast);
                if (err != OK)
                        return err;
        }

        /* Needs to happen after resolve, so that the package_names are set on
         * everything in the stack. */
        ubik_dbgsym_attach(job->ast, &job->request->region);

        infer_ctx.req = job->request;
        infer_ctx.debug = cenv->debug;
        infer_ctx.type_system = cenv->type_system;
        err = ubik_infer(job->ast, &infer_ctx);
        if (err != OK)
                return err;

        err = ubik_interfaces_compile_all(job->ast, job->request);
        if (err != OK)
                return err;
        if (cenv->debug)
        {
                printf("\ninterfaces\n");
                err = ubik_ast_print(job->ast);
                if (err != OK)
                        return err;
        }

        err = ubik_patterns_compile_all(job->ast, job->request);
        if (err != OK)
                return err;

        if (cenv->debug)
        {
                printf("\npatterns\n");
                err = ubik_ast_print(job->ast);
                if (err != OK)
                        return err;
        }

        err = ubik_adt_bind_all_to_ast(job->ast, job->request);
        if (err != OK)
                return err;

        if (cenv->debug)
        {
                printf("\nadt bindings\n");
                err = ubik_ast_print(job->ast);
                if (err != OK)
                        return err;
        }

        err = ubik_resolve(job->ast, job->request);
        if (err != OK)
                return err;
        if (cenv->debug)
        {
                printf("\npost-resolved\n");
                err = ubik_ast_print(job->ast);
                if (err != OK)
                        return err;
        }

        err = ubik_gen_graphs(job->ast, job->request);
        if (err != OK)
                return err;

        ubik_alloc1(&res, struct ubik_compile_result, &job->request->region);
        res->request = job->request;
        res->ast = job->ast;

        err = ubik_testing_run(res);
        if (err != OK)
                return err;

        if (job->request->cb != NULL)
        {
                err = job->request->cb(res);
                if (err != OK)
                        return err;
        }

        job->status = COMPILE_DONE;
        err = ubik_vector_append(&cenv->compiled, res);
        if (err != OK)
                return err;
        return OK;
}

no_ignore ubik_error
ubik_compile_enqueue(
        struct ubik_compile_env *cenv,
        struct ubik_compile_request *userreq)
{
        struct ubik_compile_request *req;
        struct ubik_compile_job *job;
        struct ubik_compile_job *check_job;
        struct ubik_compile_result *check_res;
        char *src;
        size_t i;

        src = userreq->source_name;
        for (i = 0; i < cenv->to_compile.n; i++)
        {
                check_job = (struct ubik_compile_job *)
                        cenv->to_compile.elems[i];
                if (strcmp(check_job->request->source_name, src) == 0)
                        return OK;
        }
        for (i = 0; i < cenv->compiled.n; i++)
        {
                check_res = (struct ubik_compile_result *)
                        cenv->compiled.elems[i];
                if (strcmp(check_res->request->source_name, src) == 0)
                        return OK;
        }

        if (userreq->feedback == NULL)
                printf("warning: compilation feedback unavailable, no feedback "
                       "stream provided\n");
        ubik_assert(userreq->workspace != NULL);

        req = calloc(1, sizeof(struct ubik_compile_request));
        if (req == NULL)
                return ubik_raise(ERR_NO_MEMORY, "enqueue compilation request");
        ubik_alloc_start(&req->region);

        req->source_name = ubik_strdup(userreq->source_name, &req->region);
        if (userreq->package_name != NULL)
                req->package_name = ubik_strdup(
                        userreq->package_name, &req->region);
        else
                req->package_name = NULL;
        req->source = userreq->source;
        req->reason = userreq->reason;
        req->cb = userreq->cb;
        req->feedback = userreq->feedback;
        req->type_system = cenv->type_system;
        req->workspace = userreq->workspace;

        ubik_alloc1(&job, struct ubik_compile_job, &req->region);
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

        dprint("checking imports for %s\n", job->request->package_name);
        import = job->ast->imports;

        while (import != NULL)
        {
                expect = import->canonical;
                dprint("    checking %s\n", expect);
                result = NULL;
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
                dprint("resume job for %s\n", job->request->source_name);

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
                        cenv->to_compile.n--;
                        break;
                }
        }

        return OK;
}
