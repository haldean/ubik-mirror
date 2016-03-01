/*
 * gen.h: expel bytecode generation
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
#include "expel/uri.h"

struct xl_gen_requires
{
        struct xl_uri *unresolved;
        struct xl_gen_requires *next;
};

enum xl_load_reason
{
        LOAD_MAIN = 1,
        LOAD_IMPORTED,
};

/* Compiles a single compilation unit down to a series of graphs. */
no_ignore xl_error
xl_compile_unit(
        struct xl_dagc ***graphs,
        size_t *n_graphs,
        struct xl_gen_requires **requires,
        struct xl_ast *ast,
        enum xl_load_reason load_reason);
