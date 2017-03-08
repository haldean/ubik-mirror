/*
 * walk.h: code for walking over ASTs
 * Copyright (C) 2017, Haldean Brown
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

#include "ubik/ast.h"
#include "ubik/types.h"

struct ubik_walk_info;

typedef ubik_error (*ast_visitor)(
        struct ubik_walk_info *, struct ubik_ast *);
typedef ubik_error (*expr_visitor)(
        struct ubik_walk_info *, struct ubik_ast_expr *);
typedef ubik_error (*type_visitor)(
        struct ubik_walk_info *, struct ubik_type *);
typedef ubik_error (*type_expr_visitor)(
        struct ubik_walk_info *, struct ubik_type_expr *);
typedef ubik_error (*iface_visitor)(
        struct ubik_walk_info *, struct ubik_ast_interface *);
typedef ubik_error (*impl_visitor)(
        struct ubik_walk_info *, struct ubik_ast_implementation *);
typedef ubik_error (*tparams_visitor)(
        struct ubik_walk_info *, struct ubik_type_params *);
typedef ubik_error (*tconstr_visitor)(
        struct ubik_walk_info *, struct ubik_type_constraints *);
typedef ubik_error (*adt_ctor_visitor)(
        struct ubik_walk_info *, struct ubik_ast_adt_ctors *);
typedef ubik_error (*test_visitor)(
        struct ubik_walk_info *, struct ubik_ast_test *);

struct ubik_walk_info
{
        ast_visitor ast;
        expr_visitor expr;
        type_visitor type;
        type_expr_visitor texpr;
        iface_visitor iface;
        impl_visitor impl;
        tparams_visitor tparam;
        tconstr_visitor tconstr;
        adt_ctor_visitor ctor;
        test_visitor test;
};

no_ignore ubik_error
ubik_walk_ast(struct ubik_ast *, struct ubik_walk_info *);

