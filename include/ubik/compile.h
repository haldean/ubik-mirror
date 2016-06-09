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
#include "ubik/ubik.h"

struct ubik_compilation_result;
struct ubik_compilation_request;

/* Type for functions that are called when compilation of a certain compilation
 * request is complete. */
typedef ubik_error (*ubik_compilation_cb)(
        const struct ubik_compilation_result *);

struct ubik_compilation_request
{
        /* the path of the source file, used for reporting errors */
        char *source_name;
        /* a stream containing the source code to compile */
        struct ubik_stream *source;
        /* the reason for this object to be compiled */
        enum ubik_load_reason reason;
        /* the callback to call once compilation is complete */
        ubik_compilation_cb cb;
};

struct ubik_compilation_result
{
        struct ubik_compilation_request *request;

        struct ubik_ast *ast;
        struct ubik_dagc **graphs;
        size_t n_graphs;
};

struct ubik_compilation_job
{
        struct ubik_compilation_request *request;
        struct ubik_ast *ast;
};

struct ubik_compilation_env
{
        char *scratch_dir;

        char **include_dirs;
        size_t n_include_dirs;

        bool debug;

        /* elements are ubik_compilation_job pointers */
        struct ubik_vector to_compile;
        /* elements are ubik_compilation_result pointers */
        struct ubik_vector compiled;
};

no_ignore ubik_error
ubik_compile_env_default(struct ubik_compilation_env *cenv);

no_ignore ubik_error
ubik_compile_env_free(struct ubik_compilation_env *cenv);

/* Enqueues the given compilation job. Note that the request is shallow-copied
 * into the environment; it is safe to free the request immediately after
 * calling ubik_compile_enqueue, but subelements on the request should remain
 * untouched until after compilation is complete. */
no_ignore ubik_error
ubik_compile_enqueue(
        struct ubik_compilation_env *cenv,
        struct ubik_compilation_request *req);

/* Runs the compiler queue, returning when the queue is empty. */
no_ignore ubik_error
ubik_compile_run(struct ubik_compilation_env *cenv);
