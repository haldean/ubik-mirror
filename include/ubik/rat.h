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

#include "ubik/rt.h"
#include "ubik/ubik.h"

/* Adds v1 and v2 and stores the result in r. */
void
ubik_rat_add(
        struct ubik_value *restrict r,
        struct ubik_value *restrict v1,
        struct ubik_value *restrict v2);

/* Subtracts v2 from v1 and stores the result in r. */
void
ubik_rat_sub(
        struct ubik_value *restrict r,
        struct ubik_value *restrict v1,
        struct ubik_value *restrict v2);