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

#include "expel/expel.h"
#include "expel/types.h"
#include "expel/value.h"

#include <stdio.h>
#include <stdlib.h>

bool
xl_type_satisfied(
        struct xl_value *constraint,
        struct xl_value *type)
{
        /* Eventually this will have to be way more complicated. */
        return xl_value_eq(constraint, type);
}

char *
xl_explain_type(struct xl_value *type)
{
        char *res;
        if (type->left.v == BASE_TYPE_WORD && type->right.v == 0)
        {
                res = calloc(5, sizeof(char));
                if (res == NULL)
                        return res;
                sprintf(res, "word");
                return res;
        }
        if (type->left.v == BASE_TYPE_PACKED && type->right.v == PACK_TYPE_CHAR)
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

no_ignore xl_error_t
xl_type_func_apply(
        struct xl_value *result,
        struct xl_value *func_type)
{
        /* This is super dumb, and is provided only as a way to see if this used
         * to be causing a crash. */
        result->tag = func_type->tag;
        result->left.v = func_type->left.v;
        result->right.v = func_type->right.v;
        return OK;
}

bool
xl_type_is_prim_word(struct xl_value *value)
{
        return value->left.v == BASE_TYPE_WORD
                || value->left.v == BASE_TYPE_SWORD
                || value->left.v == BASE_TYPE_BOOL;
}

no_ignore xl_error_t
xl_type_word(struct xl_value *value)
{
        value->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
        value->left.v = BASE_TYPE_WORD;
        value->right.v = 0;
        return OK;
}

no_ignore xl_error_t
xl_type_string(struct xl_value *value)
{
        value->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
        value->left.v = BASE_TYPE_PACKED;
        value->right.v = PACK_TYPE_CHAR;
        return OK;
}

no_ignore xl_error_t
xl_type_bool(struct xl_value *value)
{
        value->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
        value->left.v = BASE_TYPE_BOOL;
        value->right.v = 0;
        return OK;
}
