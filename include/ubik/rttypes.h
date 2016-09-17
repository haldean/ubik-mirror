/*
 * rttypes.h: runtime type system for ubik
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

#include <stdbool.h>

#include "ubik/ubik.h"

bool
ubik_type_satisfied(
        struct ubik_value *constraint,
        struct ubik_value *type);

/* Creates a type object that is the result of applying a single argument to the
 * given function type. */
no_ignore ubik_error
ubik_type_func_apply(
        struct ubik_value *result,
        struct ubik_value *func_type);

/* Given the value implementation of a function, returns the graph appropriate
 * for an argument of the given type. */
no_ignore ubik_error
ubik_type_match_polyfunc(
        struct ubik_value **result,
        struct ubik_value *def,
        struct ubik_value *arg_type);

char *
ubik_type_explain(struct ubik_value *type);

no_ignore ubik_error
ubik_type_rat(struct ubik_value *value);

no_ignore ubik_error
ubik_type_str(struct ubik_value *value);

no_ignore ubik_error
ubik_type_boo(struct ubik_value *value);

no_ignore ubik_error
ubik_type_tuple(
        struct ubik_value *product,
        struct ubik_value **field_types,
        size_t n_field_types);

no_ignore ubik_error
ubik_type_builtin_from_name(
        struct ubik_value *value,
        char *name);
