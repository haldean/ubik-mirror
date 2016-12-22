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
#include "ubik/alloc.h"
#include "ubik/ast.h"
#include "ubik/env.h"
#include "ubik/typesystem.h"
#include "ubik/ubik.h"
#include "ubik/vector.h"
#include <stdbool.h>

struct ubik_hook
{
        char *name;
        ubik_word arity;
        char *type_string;
        struct ubik_type_expr *type_record;
        ubik_graph_evaluator_t eval;
};

extern struct ubik_vector ubik_hooks;

typedef ubik_error(*ubik_hook_installer)(
        struct ubik_vector *, struct ubik_alloc_region *);
typedef void(*ubik_hook_uninstaller)();

no_ignore ubik_error
ubik_hooks_load(char *path);

no_ignore ubik_error
ubik_hooks_register(
        struct ubik_env *env,
        struct ubik_workspace *ws);

/* Called to store type objects on each native function in the function
 * table. Must be called before any operation that checks for native
 * function types is used. */
no_ignore ubik_error
ubik_hooks_cache_types();

/* Frees all runtime information stored about native functions. Consider
 * the runtime unusable after calling this. */
void
ubik_hooks_teardown();

/* Puts the type of the given native function in the result object.
 *
 * If the function is not defined, returns ERR_ABSENT. If the function
 * is defined but its type is not, returns ERR_UNKNOWN_TYPE. */
no_ignore ubik_error
ubik_natives_get_type(
        struct ubik_type_expr *res,
        char *func_name,
        struct ubik_alloc_region *r);

/* Returns true if the provided name is the name of a native function. */
bool
ubik_natives_is_defined(char *func_name);
