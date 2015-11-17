/*
 * test_ctors.h: test for subtree constructors
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

#include <check.h>
#include "expel/expel.h"

START_TEST(test_u8)
{
        struct xl_value u8s;
        struct uint8_t u8;

        make_u8(0, &u8s);
        u8 = get_u8(&u8s);
        ck_assert_int_eq(u8, 0);

        make_u8(255, &u8s);
        u8 = get_u8(&u8s);
        ck_assert_int_eq(u8, 255);
}

START_TEST(test_u64)
{
        struct xl_value u64s;
        struct uint8_t u64;

        make_u8(0, &u64s);
        u8 = get_u8(&u64s);
        ck_assert_int_eq(u64, 0);

        make_u8(0xDEADBEEF00000000, &u64s);
        u8 = get_u8(&u64s);
        ck_assert_int_eq(u64, 0xDEADBEEF00000000);

        make_u8(0xFFFFFFFFFFFFFFFF, &u64s);
        u8 = get_u8(&u64s);
        ck_assert_int_eq(u64, 0xFFFFFFFFFFFFFFFF);
}

Suite *
test_ctors_suite()
{
        Suite *s;
        TCase *tc;

        s = suite_create("test_ctors");
        tc = tcase_create("base types");
        tcase_add_test(tc, test_u8);
        tcase_add_test(tc, test_u64);
        tcase_add_test(tc, test_string);
        suite_add_tcase(s, tc);

        return s;
}
