/*
 * adt.c: tests for abstract data types
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

#include <stdlib.h>

#include "ubik/adt.h"
#include "ubik/ast.h"
#include "ubik/util.h"
#include "unit.h"

test_t simple_adt()
{
        struct ubik_ast_type decl = {0};
        struct ubik_ast_adt_ctors *ctors;

        ctors = NULL;

        ctors = calloc(3, sizeof(struct ubik_ast_adt_ctors));

        ctors[0].name = "Sphere";
        ctors[0].params = calloc(1, sizeof(struct ubik_ast_type_list));
        ctors[0].params[0].type_expr = calloc(1, sizeof(struct ubik_ast_type_list));
        ctors[0].params[0].type_expr->name = "Location";
        ctors[0].params[0].type_expr->type_expr_type = TYPE_EXPR_ATOM;
        ctors[0].params[0].next = NULL;
        ctors[0].next = NULL;

        decl.name = "Shape";
        decl.type = TYPE_ADT;
        decl.adt.params = NULL;
        decl.adt.constraints = NULL;
        decl.adt.ctors = ctors;

        unused(decl);

        free(ctors[0].params[0].type_expr);
        free(ctors[0].params);
        free(ctors);

        return ok;
}

run_single(simple_adt);
