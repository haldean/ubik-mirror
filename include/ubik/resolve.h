/*
 * resolve.h: name resolution at compile time
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
#include "ubik/alloc.h"
#include "ubik/ast.h"
#include "ubik/ubik.h"
#include "ubik/vector.h"

enum ubik_resolve_type
{
        /* if a name resolves locally, then the name should be accessed by using
         * a ref node to the appropriate node for the name. */
        RESOLVE_LOCAL = 1,
        /* if a name resolves globally, then the name should be accessed by
         * using a load node with the userdef scope. */
        RESOLVE_GLOBAL,
        /* if a name resolves through closure, then the ast needs to be
         * transformed such that the name can be accessed as a local. */
        RESOLVE_CLOSURE,
        /* if a name resolves globally, then the name should be accessed by
         * using a load node with the native scope. */
        RESOLVE_NATIVE,
};

struct ubik_resolve_name
{
        char *name;
        enum ubik_resolve_type type;
        struct ubik_dagc_node *node;
};

struct ubik_resolve_name_loc
{
        enum ubik_resolve_type type;
        /* The name of the package that contains this name. This should be the
         * source of the URI used to access this name, if applicable. */
        char *package_name;
};

/* Scope boundaries are used to determine whether something is
 * accessible in the current scope or not; it is useful for the closure
 * transformation to know about things in enclosing but inaccessible
 * scopes. If a scope has a BOUNDARY_BLOCK boundary type, then its
 * parent is accessible (as it's just a scope object inside another
 * scope). If a scope has a BOUNDARY_FUNCTION boundary type, its parent
 * is not accessible as they exist in separate functions, and we need to
 * close over its parents' values. If a scope has a BOUNDARY_GLOBAL boundary
 * type, its members can be accessed using load nodes instead of ref nodes and
 * thus they don't have to be closed over. */
enum ubik_resolve_boundary_type
{
        BOUNDARY_BLOCK = 1,
        BOUNDARY_GLOBAL,
        BOUNDARY_FUNCTION,
};

struct ubik_resolve_scope
{
        /* members of are struct ubik_resolve_name pointers */
        struct ubik_vector names;

        struct ubik_resolve_scope *parent;
        enum ubik_resolve_boundary_type boundary;

        /* This thing is complicated. There's a long explanation in closure.c
         * about what it means. Go read that. */
        bool needs_closure_appl;

        /* The package that this scope exists inside. */
        char *package_name;
};

enum ubik_resolve_error_type
{
        RESOLVE_ERR_NAME_NOT_FOUND = 1,
};

struct ubik_resolve_error
{
        enum ubik_resolve_error_type err_type;
        char *name;
        struct ubik_ast_loc loc;
};

struct ubik_resolve_context
{
        struct ubik_resolve_scope *native_scope;
        struct ubik_vector errors;
        struct ubik_alloc_region *region;
        struct ubik_stream *feedback;
};

no_ignore ubik_error
ubik_resolve(
        struct ubik_ast *ast,
        struct ubik_resolve_context *ctx);

void
ubik_resolve_context_free(struct ubik_resolve_context *ctx);
