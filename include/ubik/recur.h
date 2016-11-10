/*
 * recur.h: handling for recursive functions
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

#pragma once
#include <stdbool.h>

#include "ubik/assign.h"
#include "ubik/ast.h"
#include "ubik/ubik.h"

/* Finds recursive applications and expands them out into a load-and-apply.
 * Essentially, transforms this:
 *     (t x t)
 * Into:
 *     (set 0x001 t)
 *     ((load 0x001) x (load 0x001))
 * For more information, see docs/recursion.txt. */
no_ignore ubik_error
ubik_recur_expand(
        struct ubik_assign_context *ctx,
        struct ubik_ast *a);
