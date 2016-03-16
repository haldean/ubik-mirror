/*
 * compile.h: expel compilation
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
#include "expel/expel.h"
#include "expel/ast.h"


struct xl_compilation_env
{
        char *scratch_dir;

        char **include_dirs;
        size_t n_include_dirs;
};

no_ignore xl_error
xl_compile_env_default(struct xl_compilation_env *cenv);

no_ignore xl_error
xl_compile_env_free(struct xl_compilation_env *cenv);

no_ignore xl_error
xl_compile(
        struct xl_dagc ***graphs,
        size_t *n_graphs,
        struct xl_stream *in_stream,
        struct xl_compilation_env *cenv);

no_ignore xl_error
xl_compile_ast(
        struct xl_dagc ***graphs,
        size_t *n_graphs,
        struct xl_ast *ast,
        struct xl_compilation_env *cenv);

