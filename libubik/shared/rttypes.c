/*
 * rttypes.c: runtime type system for ubik
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

#include "ubik/rttypes.h"
#include "ubik/ubik.h"
#include "ubik/util.h"
#include "ubik/value.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

bool
ubik_type_satisfied(
        struct ubik_value *constraint,
        struct ubik_value *type)
{
        /* Eventually this will have to be way more complicated. */
        return ubik_value_eq(constraint, type);
}

char *
ubik_type_explain(struct ubik_value *type)
{
        unused(type);
        return "TODO: type explain";
}

no_ignore ubik_error
ubik_type_func_apply(
        struct ubik_value *result,
        struct ubik_value *func_type)
{
        unused(result);
        unused(func_type);
        return ubik_raise(ERR_NOT_IMPLEMENTED, "runtime types");
}

no_ignore ubik_error
ubik_type_match_polyfunc(
        struct ubik_value **result,
        struct ubik_value *def,
        struct ubik_value *arg_type)
{
        unused(result);
        unused(def);
        unused(arg_type);
        return ubik_raise(ERR_NOT_IMPLEMENTED, "runtime types");
}

#define failing_ctor(n)                                                 \
        no_ignore ubik_error ubik_type_##n(struct ubik_value *v) {      \
                v->type = UBIK_TYP;                                     \
                return ubik_raise(                                      \
                        ERR_NOT_IMPLEMENTED, "runtime types");          \
        }

failing_ctor(word)
failing_ctor(string)
failing_ctor(bool)
failing_ctor(float)

no_ignore ubik_error
ubik_type_tuple(
        struct ubik_value *product,
        struct ubik_value **field_types,
        size_t n_field_types)
{
        unused(product);
        unused(field_types);
        unused(n_field_types);
        return ubik_raise(ERR_NOT_IMPLEMENTED, "runtime types");
}

no_ignore ubik_error
ubik_type_builtin_from_name(
        struct ubik_value *value,
        char *name)
{
        ubik_error err;

        if (strcmp(name, "Word") == 0)
        {
                err = ubik_type_word(value);
                return err;
        }
        if (strcmp(name, "String") == 0)
        {
                err = ubik_type_string(value);
                return err;
        }
        if (strcmp(name, "Float") == 0)
        {
                err = ubik_type_float(value);
                return err;
        }

        return ubik_raise(ERR_ABSENT, "type name not builtin");
}
