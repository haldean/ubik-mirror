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

#include "ubik/ubik.h"
#include "ubik/types.h"
#include "ubik/value.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

bool
xl_type_satisfied(
        struct xl_value *constraint,
        struct xl_value *type)
{
        /* Eventually this will have to be way more complicated. */
        return xl_value_eq(constraint, type);
}

char *
xl_type_explain(struct xl_value *type)
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

no_ignore xl_error
xl_type_func_apply(
        struct xl_value *result,
        struct xl_value *func_type)
{
        /* This is super dumb, and is provided only as a way to see if this used
         * to be causing a crash. */
        result->tag = func_type->tag;
        result->left.w = func_type->left.w;
        result->right.w = func_type->right.w;
        return OK;
}

no_ignore xl_error
xl_type_match_polyfunc(
        struct xl_dagc **result,
        struct xl_value *def,
        struct xl_value *arg_type)
{
        /* Polymorphic functions are defined as a list of pairs, where the left
         * of the pair is the type node and the right of the pair is the graph
         * to call. */
        struct xl_value *pair;

        for (;;)
        {
                if (!(def->tag & TAG_LEFT_NODE))
                        return xl_raise(ERR_BAD_TAG, "bad tag in poly root");

                pair = def->left.t;
                if (pair->tag != (TAG_VALUE | TAG_LEFT_NODE | TAG_RIGHT_GRAPH))
                        return xl_raise(ERR_BAD_TAG, "bad tag in poly pair");

                if (xl_type_satisfied(pair->left.t, arg_type))
                {
                        *result = pair->right.g;
                        return OK;
                }

                if (!(def->tag & TAG_RIGHT_NODE))
                        return xl_raise(ERR_BAD_TYPE, "no func matches type");
                def = def->right.t;
        }
}

bool
xl_type_is_prim_word(struct xl_value *value)
{
        return value->left.w == BASE_TYPE_WORD
                || value->left.w == BASE_TYPE_SWORD
                || value->left.w == BASE_TYPE_BOOL
                || value->left.w == BASE_TYPE_FLOAT;
}

no_ignore xl_error
xl_type_word(struct xl_value *value)
{
        value->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
        value->left.w = BASE_TYPE_WORD;
        value->right.w = 0;
        return OK;
}

no_ignore xl_error
xl_type_string(struct xl_value *value)
{
        value->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
        value->left.w = BASE_TYPE_PACKED;
        value->right.w = PACK_TYPE_CHAR;
        return OK;
}

no_ignore xl_error
xl_type_bool(struct xl_value *value)
{
        value->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
        value->left.w = BASE_TYPE_BOOL;
        value->right.w = 0;
        return OK;
}

no_ignore xl_error
xl_type_float(struct xl_value *value)
{
        value->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
        value->left.w = BASE_TYPE_FLOAT;
        value->right.w = 0;
        return OK;
}

no_ignore xl_error
xl_type_tuple(
        struct xl_value *product,
        struct xl_value **field_types,
        size_t n_field_types)
{
        xl_error err;
        size_t i;

        product->tag |= TAG_LEFT_WORD | TAG_RIGHT_NODE;
        product->left.w = BASE_TYPE_TUPLE;

        for (i = 0; i < n_field_types; i++)
        {
                product->tag |= TAG_RIGHT_NODE;
                err = xl_value_new(&product->right.t);
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

no_ignore xl_error
xl_type_builtin_from_name(
        struct xl_value *value,
        char *name)
{
        xl_error err;

        if (strcmp(name, "Word") == 0)
        {
                err = xl_type_word(value);
                return err;
        }
        if (strcmp(name, "String") == 0)
        {
                err = xl_type_string(value);
                return err;
        }
        if (strcmp(name, "Float") == 0)
        {
                err = xl_type_float(value);
                return err;
        }

        return xl_raise(ERR_ABSENT, "type name not builtin");
}
