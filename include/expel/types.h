/*
 * types.h: runtime type system for expel
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

#include "expel/expel.h"

bool
xl_type_satisfied(
        struct xl_value *constraint,
        struct xl_value *type);

/* Creates a type object that is the result of applying a single argument to the
 * given function type. */
no_ignore xl_error
xl_type_func_apply(
        struct xl_value *result,
        struct xl_value *func_type);

/* Given the value implementation of a function, returns the graph appropriate
 * for an argument of the given type. */
no_ignore xl_error
xl_type_match_polyfunc(
        struct xl_dagc **result,
        struct xl_value *def,
        struct xl_value *arg_type);

char *
xl_type_explain(struct xl_value *type);

/* Returns true if the values of the given type are "primitive words", meaning
 * that their representation is entirely contained in the left value of their
 * associated node. */
bool
xl_type_is_prim_word(struct xl_value *value);

no_ignore xl_error
xl_type_word(struct xl_value *value);

no_ignore xl_error
xl_type_string(struct xl_value *value);

no_ignore xl_error
xl_type_bool(struct xl_value *value);

no_ignore xl_error
xl_type_float(struct xl_value *value);

no_ignore xl_error
xl_type_tuple(
        struct xl_value *product,
        struct xl_value **field_types,
        size_t n_field_types);

no_ignore xl_error
xl_type_builtin_from_name(
        struct xl_value *value,
        char *name);
