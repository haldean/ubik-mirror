/*
 * typ.h: utilities for working with TYP objects
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

#include "ubik/rt.h"
#include "ubik/typesystem.h"
#include "ubik/ubik.h"

/* Create a TYP value from an AST type definition. Types referenced by the
   expression must already be present in the given type system. If the res
   pointer isn't NULL, this fills in the provided value instead of creating a
   new value. */
no_ignore ubik_error
ubik_typ_from_ast(
        struct ubik_value **res,
        struct ubik_type *t,
        struct ubik_typesystem *tsys,
        struct ubik_workspace *ws);
