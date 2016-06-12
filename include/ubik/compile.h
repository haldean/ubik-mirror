/*
 * compile.h: ubik compilation
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

#pragma once
#include "ubik/ast.h"
#include "ubik/gen.h"
#include "ubik/stream.h"
#include "ubik/ubik.h"

enum ubik_compile_job_status
{
        COMPILE_WAIT_FOR_AST = 1,
        COMPILE_WAIT_FOR_IMPORTS,
        COMPILE_READY,
        COMPILE_DONE,
};

struct ubik_compile_result;
struct ubik_compile_request;

/* Type for functions that are called when compilation of a certain compilation
 * request is complete. */
typedef ubik_error (*ubik_compile_cb)(
        const struct ubik_compile_result *);

/* Represents a user's request for compilation. */
struct ubik_compile_request
{
        /* the path of the source file, used for reporting errors */
        char *source_name;
        /* the package that this represents (only required if this is being
         * enqueued to satisfy an import) */
        char *package_name;
        /* a stream containing the source code to compile */
        struct ubik_stream source;
        /* the reason for this object to be compiled */
        enum ubik_load_reason reason;
        /* the callback to call once compilation is complete */
        ubik_compile_cb cb;
};

/* Represents the final result of compilation. */
struct ubik_compile_result
{
        /* the request that was handled */
        struct ubik_compile_request *request;
        /* the fully-annotated AST for the request */
        struct ubik_ast *ast;
        /* a list of graphs required for the request and its transitive
         * dependency closure */
        struct ubik_dagc **graphs;
        size_t n_graphs;
};

/* Represents an in-progress compilation job. */
struct ubik_compile_job
{
        /* the request that kicked off the job */
        struct ubik_compile_request *request;
        /* the loaded AST for the source (stored here because jobs are
         * executed in two phases: loading and compiling, with
         * compilation of imported packages occurring in between) */
        struct ubik_ast *ast;
        /* the status of the job */
        enum ubik_compile_job_status status;
        /* the graphs created by compiling its dependencies */
        struct ubik_vector dep_graphs;
};

struct ubik_compile_env
{
        char *scratch_dir;

        char **include_dirs;
        size_t n_include_dirs;

        bool debug;

        /* elements are ubik_compile_job pointers */
        struct ubik_vector to_compile;
        /* elements are ubik_compile_result pointers */
        struct ubik_vector compiled;
};

no_ignore ubik_error
ubik_compile_env_default(struct ubik_compile_env *cenv);

no_ignore ubik_error
ubik_compile_env_free(struct ubik_compile_env *cenv);

/* Enqueues the given compilation job. Note that the request is shallow-copied
 * into the environment; it is safe to free the request immediately after
 * calling ubik_compile_enqueue, but subelements on the request should remain
 * untouched until after compilation is complete. */
no_ignore ubik_error
ubik_compile_enqueue(
        struct ubik_compile_env *cenv,
        struct ubik_compile_request *req);

/* Runs the compiler queue, returning when the queue is empty. */
no_ignore ubik_error
ubik_compile_run(struct ubik_compile_env *cenv);
