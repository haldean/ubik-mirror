/*
 * unit.h: header-only unit testing "library"
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

#include <stdio.h>

typedef struct
{
        char *msg;
        int line;
} test_t;

#define assert(x) if (!(x)) return (test_t){.msg = #x, .line = __LINE__}
#define run(x) { \
        test_t __unit_res = (x)(); __n_tests++; \
        if (__unit_res.msg != NULL) { \
                printf("fail: %s line %d: %s\n", #x, __unit_res.line, __unit_res.msg); \
                __n_errs++;\
        } else printf("ok:   %s\n", #x); }
#define init() int __n_errs = 0, __n_tests = 0;
#define finish() \
        if (__n_errs != 0) \
                printf("%d of %d tests succeeded\n", __n_tests - __n_errs, __n_tests); \
        return __n_errs;
#define ok (test_t){.msg = NULL, .line = __LINE__}

#define run_single(test) int main() { \
        xl_error err = xl_start(); \
        if (err != OK) { \
                printf("couldn't start expel: %s\n", xl_error_explain(err)); \
                return 1; \
        } \
        init(); run(test); \
        err = xl_teardown(); \
        if (err != OK) { \
                printf("couldn't close expel: %s\n", xl_error_explain(err)); \
                return 1; \
        } \
        finish(); }
