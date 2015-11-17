/*
 * ctors.c: constructors for various tree structures
 * Copyright (C) 2015, Haldean Brown
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

#include <stdlib.h>

void
make_u8(uint8_t val, struct xl_value *out)
{
        struct xl_type *type;

        type = malloc(sizeof(struct xl_type));
        type->base = BASE_TYPE_U8;

        out->tag = TAG_TYPED;
        out->left.t = type;
        out->right.v = val;
}

uint8_t
get_u8(struct xl_value *out)
{
        return (uint8_t) out->right.v;
}

void
make_u64(uint64_t val, struct xl_value *out)
{
        struct xl_type *type;

        type = malloc(sizeof(struct xl_type));
        type->base = BASE_TYPE_U64;

        out->tag = TAG_TYPED;
        out->left.t = type;
        out->right.v = val;
}

uint64_t
get_u64(struct xl_value *out)
{
        return out->right.v;
}

void
make_string(const char * val, struct xl_value *out)
{
        (void)val;
        (void)out;
}

const char *
get_string(struct xl_value *out)
{
        (void)out;
        return NULL;
}
