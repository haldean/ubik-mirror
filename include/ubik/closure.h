/*
 * closure.h: closure transformation on ASTs
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
#include "ubik/ubik.h"
#include "ubik/resolve.h"

/* Takes an AST and transforms closures into partially-applied functions. This
 * transformation is explained in-depth in the "Code generation" portion of the
 * architecture document. */
no_ignore ubik_error
ubik_reduce_closures(
        struct ubik_resolve_context *ctx,
        struct ubik_ast *ast);
