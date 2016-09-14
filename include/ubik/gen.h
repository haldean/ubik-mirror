/*
 * gen.h: ubik bytecode generation
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
#include "ubik/assign.h"
#include "ubik/ast.h"
#include "ubik/compile.h"
#include "ubik/ubik.h"
#include "ubik/uri.h"

/* Compiles a single compilation unit down to a series of graphs.
 * Just one graph is returned here: the modinit graph. All other graphs are
 * referenced by it, and are introduced into the environment by evaluating the
 * modinit graph.
 *
 * The region provided is used for scratch memory only; the graphs that are
 * returned are allocated by the graph allocator and are refcounted. */
no_ignore ubik_error
ubik_gen_graphs(
        struct ubik_value **res,
        struct ubik_ast *ast,
        struct ubik_compile_request *req);
