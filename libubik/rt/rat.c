/*
 * rat.h: utilities for working with rational values
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

#include "ubik/rat.h"

/* TODO: overflow!? */
static inline ubik_word
lcm(ubik_word w0, ubik_word w1)
{
        ubik_word t;
        ubik_word prod;

        prod = w0 * w1;

        while (w1 != 0)
        {
                t = w1;
                w1 = w0 % w1;
                w0 = t;
        }
        return prod / w0;
}

void
ubik_rat_add(
        struct ubik_value *restrict r,
        struct ubik_value *restrict v1,
        struct ubik_value *restrict v2)
{
        r->type = UBIK_RAT;
        r->rat.den = lcm(v1->rat.den, v2->rat.den);
        r->rat.num =
                v1->rat.num * (r->rat.den / v1->rat.den) +
                v2->rat.num * (r->rat.den / v2->rat.den);
}

void
ubik_rat_sub(
        struct ubik_value *restrict r,
        struct ubik_value *restrict v1,
        struct ubik_value *restrict v2)
{
        r->type = UBIK_RAT;
        r->rat.den = lcm(v1->rat.den, v2->rat.den);
        r->rat.num =
                v1->rat.num * (r->rat.den / v1->rat.den) -
                v2->rat.num * (r->rat.den / v2->rat.den);
}
