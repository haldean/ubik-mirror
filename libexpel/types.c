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

bool
xl_type_satisfied(
        struct xl_value *constraint,
        struct xl_value *type)
{
        /* Eventually this will have to be way more complicated. */
        return xl_value_eq(constraint, type);
}

no_ignore xl_error_t
xl_type_word(struct xl_value *value)
{
        value->left.v = BASE_TYPE_WORD;
        value->right.v = 0;
        return OK;
}

no_ignore xl_error_t
xl_type_string(struct xl_value *value)
{
        value->left.v = BASE_TYPE_PACKED;
        value->right.v = PACK_TYPE_CHAR;
        return OK;
}
