/*
 * patterns.h: compiles patterns to predicates
 * Copyright (C) 2016, Haldean Brown
 *
 * As described in docs/patterns.txt, pattern blocks are compiled down
 * to predicate blocks that use private, unsafe APIs (that are not
 * normally accessible to user code) to unpack ADTs after the compiler
 * has proven that it is safe to use these native functions. This logic
 * implements the pass of the compiler that takes pattern blocks and
 * syntactically transforms them into predicate blocks, which can then
 * be code generated very simply.
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
#include "ubik/ubik.h"

struct ubik_patterns_context
{
        void *unused;
};

/* Transforms pattern blocks into equivalent predicate blocks. */
no_ignore ubik_error
ubik_patterns_compile_all(
        struct ubik_ast *ast,
        struct ubik_patterns_context *ctx);

void
ubik_patterns_context_free(struct ubik_patterns_context *c);
