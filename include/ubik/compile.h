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
#include "ubik/ubik.h"
#include "ubik/ast.h"


struct ubik_compilation_env
{
        char *scratch_dir;

        char **include_dirs;
        size_t n_include_dirs;
};

no_ignore ubik_error
ubik_compile_env_default(struct ubik_compilation_env *cenv);

no_ignore ubik_error
ubik_compile_env_free(struct ubik_compilation_env *cenv);

no_ignore ubik_error
ubik_compile(
        struct ubik_dagc ***graphs,
        size_t *n_graphs,
        char *source_name,
        struct ubik_stream *in_stream,
        struct ubik_compilation_env *cenv);

no_ignore ubik_error
ubik_compile_ast(
        struct ubik_dagc ***graphs,
        size_t *n_graphs,
        struct ubik_ast *ast,
        struct ubik_compilation_env *cenv);
