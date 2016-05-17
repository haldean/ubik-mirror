/*
 * adt.c: utilities for creating and instantiating algebraic data types
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

#include <string.h>

#include "ubik/adt.h"
#include "ubik/assert.h"
#include "ubik/list.h"
#include "ubik/util.h"
#include "ubik/value.h"

no_ignore ubik_error
ubik_adt_instantiate(
        struct ubik_value **res,
        struct ubik_value *type_decl,
        struct ubik_value *args)
{
        unused(type_decl);
        *res = args;
        return OK;
}

no_ignore ubik_error
ubik_adt_get_ctor(char **res, struct ubik_value *value)
{
        struct ubik_value *encoded;
        ubik_error err;
        char *name;
        size_t read;

        err = ubik_list_get(&encoded, value, 0);
        if (err != OK)
                return err;

        err = ubik_string_read(&name, &read, encoded);
        if (err != OK)
                return err;

        *res = name;
        return OK;
}

no_ignore ubik_error
ubik_adt_get_field(
        struct ubik_value **res,
        struct ubik_value *instance,
        size_t n)
{
        ubik_error err;

        err = ubik_list_get(res, instance, n + 1);
        if (err != OK)
                return err;

        return OK;
}

no_ignore ubik_error
ubik_adt_inst_size(
        size_t *n,
        struct ubik_value *instance)
{
        size_t size;
        ubik_error err;

        err = ubik_list_size(&size, instance);
        if (err != OK)
                return err;

        if (size < 1)
                return ubik_raise(
                        ERR_BAD_VALUE,
                        "provided value is not an ADT instance");
        *n = size - 1;
        return OK;
}

no_ignore ubik_error
ubik_adt_create_decl(
        struct ubik_value *res,
        struct ubik_ast_type *source)
{
        struct ubik_ast_type_params *src_params;
        struct ubik_ast_type_constraints *src_constraints;
        struct ubik_ast_adt_ctors *src_ctors;

        struct ubik_value *dst_params;
        struct ubik_value *dst_constraints;
        struct ubik_value *dst_ctors;
        struct ubik_value *dst_ctor;

        struct ubik_value *t;
        ubik_error err;

        err = ubik_value_new(&dst_params);
        if (err != OK)
                return err;
        err = ubik_list_create_empty(dst_params);
        if (err != OK)
                return err;

        src_params = source->adt.params;
        if (src_params != NULL)
                return ubik_raise(ERR_NOT_IMPLEMENTED, "no params on ADTs yet");

        err = ubik_value_new(&dst_constraints);
        if (err != OK)
                return err;
        err = ubik_list_create_empty(dst_constraints);
        if (err != OK)
                return err;

        src_constraints = source->adt.constraints;
        if (src_constraints != NULL)
                return ubik_raise(
                        ERR_NOT_IMPLEMENTED, "no constraints on ADTs yet");

        err = ubik_value_new(&dst_ctors);
        if (err != OK)
                return err;
        err = ubik_list_create_empty(dst_ctors);
        if (err != OK)
                return err;

        for (src_ctors = source->adt.ctors;
                src_ctors != NULL; src_ctors = src_ctors->next)
        {
                err = ubik_value_new(&dst_ctor);
                if (err != OK)
                        return err;
                err = ubik_list_create_empty(dst_ctor);
                if (err != OK)
                        return err;

                err = ubik_value_new(&t);
                if (err != OK)
                        return err;
                err = ubik_value_pack_string(
                        t, src_ctors->name, strlen(src_ctors->name));
                if (err != OK)
                        return err;
                err = ubik_list_append(dst_ctor, t);
                if (err != OK)
                        return err;
                err = ubik_release(t);
                if (err != OK)
                        return err;

                err = ubik_list_append(dst_ctors, dst_ctor);
                if (err != OK)
                        return err;
                err = ubik_release(dst_ctor);
                if (err != OK)
                        return err;
        }

        err = ubik_list_create_empty(res);
        if (err != OK)
                return err;

        err = ubik_list_append(res, dst_params);
        if (err != OK)
                return err;
        err = ubik_release(dst_params);
        if (err != OK)
                return err;

        err = ubik_list_append(res, dst_constraints);
        if (err != OK)
                return err;
        err = ubik_release(dst_constraints);
        if (err != OK)
                return err;

        err = ubik_list_append(res, dst_ctors);
        if (err != OK)
                return err;
        err = ubik_release(dst_ctors);
        if (err != OK)
                return err;
        return OK;
}

no_ignore ubik_error
ubik_adt_create_constructor(
        struct ubik_dagc **res,
        struct ubik_value *type_decl,
        char *constructor_name)
{
        struct ubik_value *check_ctor;
        struct ubik_value *c;
        char *test_name;
        size_t test_n;
        bool found;
        ubik_error err;

        err = ubik_list_get(&check_ctor, type_decl, 2);
        if (err != OK)
                return err;

        found = false;

        while ((check_ctor->tag & TAG_LEFT_WORD) != 0)
        {
                err = ubik_list_get(&c, check_ctor->left.t, 0);
                if (err != OK)
                        return err;

                err = ubik_string_read(&test_name, &test_n, c);
                if (err != OK)
                        return err;

                if (strcmp(test_name, constructor_name) == 0)
                {
                        found = true;
                        break;
                }

                if ((check_ctor->tag & TAG_RIGHT_NODE) == 0)
                        return ubik_raise(
                                ERR_BAD_TAG, "bad tag in ctor definition");
                check_ctor = check_ctor->right.t;
        }

        if (!found)
                return ubik_raise(ERR_ABSENT, "ctor does not exist");

        c = check_ctor->left.t;

        unused(res);
        return OK;
}
