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
        char *res;
        if (type->left.w == BASE_TYPE_WORD && type->right.w == 0)
        {
                res = calloc(5, sizeof(char));
                if (res == NULL)
                        return res;
                sprintf(res, "word");
                return res;
        }
        if (type->left.w == BASE_TYPE_PACKED && type->right.w == PACK_TYPE_CHAR)
        {
                res = calloc(7, sizeof(char));
                if (res == NULL)
                        return res;
                sprintf(res, "string");
                return res;
        }
        res = calloc(8, sizeof(char));
        if (res == NULL)
                return res;
        sprintf(res, "unknown");
        return res;
}

no_ignore ubik_error
ubik_type_func_apply(
        struct ubik_value *result,
        struct ubik_value *func_type)
{
        /* This is super dumb, and is provided only as a way to see if this used
         * to be causing a crash. */
        result->tag = func_type->tag;
        result->left.w = func_type->left.w;
        result->right.w = func_type->right.w;
        return OK;
}

no_ignore ubik_error
ubik_type_match_polyfunc(
        struct ubik_dagc **result,
        struct ubik_value *def,
        struct ubik_value *arg_type)
{
        /* Polymorphic functions are defined as a list of pairs, where the left
         * of the pair is the type node and the right of the pair is the graph
         * to call. */
        struct ubik_value *pair;

        for (;;)
        {
                if (!(def->tag & TAG_LEFT_NODE))
                        return ubik_raise(ERR_BAD_TAG, "bad tag in poly root");

                pair = def->left.t;
                if (pair->tag != (TAG_VALUE | TAG_LEFT_NODE | TAG_RIGHT_GRAPH))
                        return ubik_raise(ERR_BAD_TAG, "bad tag in poly pair");

                if (ubik_type_satisfied(pair->left.t, arg_type))
                {
                        *result = pair->right.g;
                        return OK;
                }

                if (!(def->tag & TAG_RIGHT_NODE))
                        return ubik_raise(ERR_BAD_TYPE, "no func matches type");
                def = def->right.t;
        }
}

bool
ubik_type_is_prim_word(struct ubik_value *value)
{
        return value->left.w == BASE_TYPE_WORD
                || value->left.w == BASE_TYPE_SWORD
                || value->left.w == BASE_TYPE_BOOL
                || value->left.w == BASE_TYPE_FLOAT;
}

no_ignore ubik_error
ubik_type_word(struct ubik_value *value)
{
        value->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
        value->left.w = BASE_TYPE_WORD;
        value->right.w = 0;
        return OK;
}

no_ignore ubik_error
ubik_type_string(struct ubik_value *value)
{
        value->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
        value->left.w = BASE_TYPE_PACKED;
        value->right.w = PACK_TYPE_CHAR;
        return OK;
}

no_ignore ubik_error
ubik_type_bool(struct ubik_value *value)
{
        value->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
        value->left.w = BASE_TYPE_BOOL;
        value->right.w = 0;
        return OK;
}

no_ignore ubik_error
ubik_type_float(struct ubik_value *value)
{
        value->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
        value->left.w = BASE_TYPE_FLOAT;
        value->right.w = 0;
        return OK;
}

no_ignore ubik_error
ubik_type_tuple(
        struct ubik_value *product,
        struct ubik_value **field_types,
        size_t n_field_types)
{
        ubik_error err;
        size_t i;

        product->tag |= TAG_LEFT_WORD | TAG_RIGHT_NODE;
        product->left.w = BASE_TYPE_TUPLE;

        for (i = 0; i < n_field_types; i++)
        {
                product->tag |= TAG_RIGHT_NODE;
                err = ubik_value_new(&product->right.t);
                if (err != OK)
                        return err;
                product = product->right.t;

                product->left.t = field_types[i];
                product->tag |= TAG_LEFT_NODE;
        }

        product->tag |= TAG_RIGHT_WORD;
        product->right.w = 0;

        return OK;
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
