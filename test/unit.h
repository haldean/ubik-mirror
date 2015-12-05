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

#define assert(x) if (!(x)) return #x
#define run(x) { \
        char * __unit_res = x(); __n_tests++; \
        if (__unit_res != NULL) { printf("fail: %s: %s\n", #x, __unit_res); __n_errs++; } \
        else printf("ok:   %s\n", #x); }
#define init() int __n_errs = 0, __n_tests = 0;
#define finish() printf("%d of %d tests failed\n", __n_errs, __n_tests); return __n_errs;
#define ok NULL
