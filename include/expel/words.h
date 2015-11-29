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

#define pack(c) ( \
    (((uint64_t) ((c)[0] == '.' ? 0 : (c)[0])) << ((7 - 0) * 8)) | \
    (((uint64_t) ((c)[1] == '.' ? 0 : (c)[1])) << ((7 - 1) * 8)) | \
    (((uint64_t) ((c)[2] == '.' ? 0 : (c)[2])) << ((7 - 2) * 8)) | \
    (((uint64_t) ((c)[3] == '.' ? 0 : (c)[3])) << ((7 - 3) * 8)) | \
    (((uint64_t) ((c)[4] == '.' ? 0 : (c)[4])) << ((7 - 4) * 8)) | \
    (((uint64_t) ((c)[5] == '.' ? 0 : (c)[5])) << ((7 - 5) * 8)) | \
    (((uint64_t) ((c)[6] == '.' ? 0 : (c)[6])) << ((7 - 6) * 8)) | \
    (((uint64_t) ((c)[7] == '.' ? 0 : (c)[7])) << ((7 - 7) * 8)))
