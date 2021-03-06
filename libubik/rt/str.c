/*
 * str.c: utilities for working with strings
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

#include "ubik/alloc.h"
#include "ubik/str.h"

void
ubik_str_concat(
        struct ubik_value *restrict r,
        struct ubik_value *restrict v1,
        struct ubik_value *restrict v2)
{
        r->type = UBIK_STR;
        r->str.length = v1->str.length + v2->str.length;
        ubik_galloc((void **) &r->str.data, sizeof(char), r->str.length);
        memcpy(r->str.data, v1->str.data, v1->str.length);
        memcpy(r->str.data + v1->str.length,
               v2->str.data, v2->str.length);
}

bool
ubik_str_eq(
        struct ubik_str *restrict s1,
        struct ubik_str *restrict s2)
{
        return s1->length == s2->length &&
                strncmp(s1->data, s2->data, s1->length) == 0;
}
