/*
 * fun.h: utilities for working with function values
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
#include "ubik/vector.h"

void
ubik_fun_from_vector(
        struct ubik_value *res,
        struct ubik_vector *nodes,
        ubik_word result);

no_ignore ubik_error
ubik_fun_get_deps(
        ubik_word *d1, ubik_word *d2, ubik_word *d3,
        struct ubik_node *n);

no_ignore ubik_error
ubik_fun_get_parents(
        struct ubik_vector *parents, struct ubik_value *graph, ubik_word node);
