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
#include "expel/expel.h"
#include "expel/ast.h"
#include "expel/vector.h"

enum xl_resolve_type
{
        /* if a name resolves locally, then the name should be accessed by using
         * a ref node to the appropriate node for the name. */
        RESOLVE_LOCAL = 1,
        /* if a name resolves globally, then the name should be accessed by
         * using a load node with the userdef scope. */
        RESOLVE_GLOBAL
};

struct xl_resolve_name
{
        char *name;
        enum xl_resolve_type type;
        struct xl_dagc_node *node;
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
enum xl_resolve_boundary_type
{
        BOUNDARY_BLOCK = 1,
        BOUNDARY_FUNCTION,
        BOUNDARY_GLOBAL
};

struct xl_resolve_scope
{
        /* members of are struct xl_resolve_name pointers */
        struct xl_vector names;

        struct xl_resolve_scope *parent;
        enum xl_resolve_boundary_type boundary;
};

no_ignore xl_error
assign_all_initial_scopes(
        struct xl_ast *ast,
        struct xl_resolve_scope *parent);

no_ignore xl_error
update_scopes_with_bindings(struct xl_ast *ast);
