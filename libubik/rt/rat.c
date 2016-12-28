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
#include "ubik/util.h"

#include <stdlib.h>
#include <string.h>

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

static inline ubik_word
gcd(ubik_word w0, ubik_word w1)
{
        ubik_word t;
        while (w1 != 0)
        {
                t = w1;
                w1 = w0 % w1;
                w0 = t;
        }
        return w0;
}

static inline void
rsimplify(struct ubik_value *r)
{
        ubik_word g;
        g = gcd(abs(r->rat.num), abs(r->rat.den));
        r->rat.num /= g;
        r->rat.den /= g;
}

static inline void
rfloor(struct ubik_value *r)
{
        /* TODO: handle overflow here! */
        r->rat.num /= (ubik_sword) r->rat.den;
        r->rat.den = 1;
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

void
ubik_rat_mul(
        struct ubik_value *restrict r,
        struct ubik_value *restrict v1,
        struct ubik_value *restrict v2)
{
        r->type = UBIK_RAT;
        r->rat.num = v1->rat.num * v2->rat.num;
        r->rat.den = v1->rat.den * v2->rat.den;
        rsimplify(r);
}

void
ubik_rat_div(
        struct ubik_value *restrict r,
        struct ubik_value *restrict v1,
        struct ubik_value *restrict v2)
{
        r->type = UBIK_RAT;
        r->rat.num = v1->rat.num * v2->rat.den;
        r->rat.den = v1->rat.den * v2->rat.num;
        rsimplify(r);
}

void
ubik_rat_mod(
        struct ubik_value *restrict r,
        struct ubik_value *restrict v1,
        struct ubik_value *restrict v2)
{
        struct ubik_value t0;
        struct ubik_value t1;

        r->type = UBIK_RAT;
        if (likely(v1->rat.den == 1 && v2->rat.den == 1)) {
                r->rat.num = v1->rat.num % v2->rat.num;
                r->rat.den = 1;
                return;
        }

        ubik_rat_div(&t0, v1, v2);
        rfloor(&t0);
        ubik_rat_mul(&t1, v2, &t0);
        ubik_rat_sub(r, v1, &t1);
}

bool
ubik_rat_lt(
        struct ubik_value *restrict v1,
        struct ubik_value *restrict v2)
{
        int64_t s1;
        int64_t s2;

        s1 = v1->rat.num * v2->rat.den;
        s2 = v2->rat.num * v1->rat.den;

        return s1 < s2;
}

no_ignore ubik_error
ubik_rat_read(
        struct ubik_rat *res,
        char *str)
{
        size_t i;
        size_t n;
        ubik_word s;
        bool neg;

        n = strlen(str);
        if (n == 0)
                return ubik_raise(
                        ERR_NO_DATA, "empty string has no number value");;

        res->num = 0;
        res->den = 1;
        neg = str[0] == '-';

        /* Check for a decimal point */
        for (i = neg ? 1 : 0; i < n; i++)
        {
                if (str[i] == '.')
                        break;
                if (str[i] < '0' || str[i] > '9')
                        return ubik_raise(
                                ERR_BAD_VALUE,
                                "found char that isn't digit or decimal");
                res->num = 10 * res->num + (str[i] - '0');
        }

        /* Add in the bits after the decimal point by multiplying the
           denominator by 10 for every digit we add. */
        for (i++; i < n; i++)
        {
                if (str[i] < '0' || str[i] > '9')
                        return ubik_raise(
                                ERR_BAD_VALUE,
                                "found char that isn't digit or decimal");
                res->num = 10 * res->num + (str[i] - '0');
                res->den *= 10;
        }

        s = gcd(res->num, res->den);
        res->num /= s;
        res->den /= s;

        /* Hold the negation off until the end, as it keeps the GCD algorithm
           simpler. */
        if (neg)
                res->num = -res->num;

        return OK;
}
