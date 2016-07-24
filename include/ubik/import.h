/*
 * import.h: packaging and importing utilities
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
#include "ubik/alloc.h"
#include "ubik/ast.h"
#include "ubik/compile.h"

/* Adds bindings for all names that are splatted into the given AST. */
no_ignore ubik_error
ubik_import_add_splats(
        struct ubik_compile_env *cenv,
        struct ubik_ast *ast,
        struct ubik_alloc_region *region);
