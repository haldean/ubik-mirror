/*
 * adt.h: utilities for working with algebraic data types
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

/* Instantiate the ADT given in the value-encoded type declaration using
 * the provided value-encoded tuple of arguments. The first argument in
 * the argument tuple should be a string whose contents are the name of
 * the constructor to use. The instantiated value is returned without a
 * reference for the caller; the caller must take it upon receipt. */
no_ignore ubik_error
ubik_adt_instantiate(
        struct ubik_value **res,
        struct ubik_value *type_decl,
        struct ubik_value *args);

/* Returns the name of the constructor that was used for the value. The
 * caller must free the result of this function. */
no_ignore ubik_error
ubik_adt_get_ctor(char **res, struct ubik_value *value);

/* Returns the Nth field of the provided ADT. The return reference is
 * mutable and mutates the contents of the instance. */
no_ignore ubik_error
ubik_adt_get_field(
        struct ubik_value **res,
        struct ubik_value *instance,
        size_t n);

/* Returns the type of the Nth field of the provided ADT. The return reference
 * is mutable and mutates the contents of the type declaration. The instance is
 * used to determine which constructor is being referenced, but it is sufficient
 * to pass a one-element list whose first element is the constructor being
 * queried. */
no_ignore ubik_error
ubik_adt_get_field_type(
        struct ubik_value **type,
        struct ubik_value *type_decl,
        struct ubik_value *instance,
        size_t n);

/* Returns the number of fields on the provided ADT instance. */
no_ignore ubik_error
ubik_adt_inst_size(
        size_t *n,
        struct ubik_value *instance);

/* Creates a value-encoded type declaration for an ADT. */
no_ignore ubik_error
ubik_adt_create_decl(
        struct ubik_value *res,
        struct ubik_ast_type *source);
