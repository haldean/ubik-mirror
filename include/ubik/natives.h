/*
 * natives.h: built-in native methods
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

#pragma once
#include "ubik/dagc.h"
#include "ubik/ubik.h"
#include <stdbool.h>

no_ignore ubik_error
ubik_natives_register(struct ubik_env *env);

/* Returns true if the provided name is the name of a native function. */
bool
ubik_natives_is_defined(char *);

/* Internal-only convenience functions for native URI definitions. */
no_ignore ubik_error
ubik_internal_native_uri(struct ubik_uri **uri, char *name);

no_ignore ubik_error
ubik_internal_native_create_op(
        struct ubik_dagc **graph_ptr,
        size_t arity,
        ubik_native_evaluator_t evaluator);

