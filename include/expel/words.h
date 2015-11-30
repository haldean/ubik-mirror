/*
 * words.h: utilities for dealing with words
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

#include <stdint.h>

#define pack(a, b, c, d, e, f, g, h) ( \
    (((uint64_t) (a)) << ((7 - 7) * 8)) | \
    (((uint64_t) (b)) << ((7 - 6) * 8)) | \
    (((uint64_t) (c)) << ((7 - 5) * 8)) | \
    (((uint64_t) (d)) << ((7 - 4) * 8)) | \
    (((uint64_t) (e)) << ((7 - 3) * 8)) | \
    (((uint64_t) (f)) << ((7 - 2) * 8)) | \
    (((uint64_t) (g)) << ((7 - 1) * 8)) | \
    (((uint64_t) (h)) << ((7 - 0) * 8)))
