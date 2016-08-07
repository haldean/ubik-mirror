/*
 * typesystem.h: maintains type information during compiliation
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
#include "ubik/alloc.h"
#include "ubik/types.h"
#include "ubik/ubik.h"

#define UBIK_MAX_INTERFACE_PARAMS 16

struct ubik_typesystem;
struct ubik_compile_request;

no_ignore ubik_error
ubik_typesystem_init(
        struct ubik_typesystem **tsys,
        struct ubik_alloc_region *region);

no_ignore ubik_error
ubik_typesystem_load(
        struct ubik_typesystem *tsys,
        struct ubik_ast *ast,
        struct ubik_compile_request *req);

no_ignore ubik_error
ubik_typesystem_unify(
        struct ubik_type_expr **unified,
        struct ubik_typesystem *tsys,
        char *package,
        struct ubik_type_expr *assign_to,
        struct ubik_type_expr *assign_from);

void
ubik_typesystem_dump(struct ubik_typesystem *tsys);
