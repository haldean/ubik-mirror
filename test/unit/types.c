/*
 * types.c: run tests on compile-time type helpers
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

#include "ubik/alloc.h"
#include "ubik/parse.h"
#include "ubik/types.h"
#include "ubik/ubik.h"
#include "unit.h"

test_t
count_args()
{
        struct ubik_type_expr *t;
        struct ubik_alloc_region r = {0};

        assert(ubik_parse_type_expr(&t, &r, "String -> Word -> Noun") == OK);
        assert(ubik_type_count_arguments(t) == 2);

        assert(ubik_parse_type_expr(&t, &r, "String") == OK);
        assert(ubik_type_count_arguments(t) == 0);

        assert(ubik_parse_type_expr(
                &t, &r, "(String -> String) -> String") == OK);
        assert(ubik_type_count_arguments(t) == 1);

        assert(ubik_parse_type_expr(
                       &t, &r, "a -> (b -> c) -> d") == OK);
        assert(ubik_type_count_arguments(t) == 2);

        assert(ubik_parse_type_expr(
                       &t, &r, "(a -> b -> c) -> d") == OK);
        assert(ubik_type_count_arguments(t) == 1);

        assert(ubik_parse_type_expr(
                       &t, &r, "(a -> b) -> c -> d") == OK);
        assert(ubik_type_count_arguments(t) == 2);

        assert(ubik_parse_type_expr(
                       &t, &r, "a -> b -> (c -> d)") == OK);
        assert(ubik_type_count_arguments(t) == 3);

        assert(ubik_parse_type_expr(
                       &t, &r, "a -> (b -> (c -> d))") == OK);
        assert(ubik_type_count_arguments(t) == 3);

        ubik_alloc_free(&r);
        return ok;
}

run_single(count_args)
