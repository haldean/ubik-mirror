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
#include <string.h>

#include "ubik/adt.h"
#include "ubik/ast.h"
#include "ubik/ubik.h"
#include "ubik/value.h"
#include "unit.h"

test_t simple_adt()
{
        struct ubik_ast_type sdecl = {0};
        struct ubik_ast_adt_ctors *ctors;
        struct ubik_value *decl;
        struct ubik_value *args;
        struct ubik_value *inst;

        char *res_str;
        size_t res_size;

        ctors = NULL;

        ctors = calloc(2, sizeof(struct ubik_ast_adt_ctors));

        ctors[0].name = "Sphere";
        ctors[0].params = calloc(1, sizeof(struct ubik_ast_type_list));
        ctors[0].params[0].type_expr = calloc(1, sizeof(struct ubik_ast_type_list));
        ctors[0].params[0].type_expr->name = "Length";
        ctors[0].params[0].type_expr->type_expr_type = TYPE_EXPR_ATOM;
        ctors[0].params[0].next = NULL;
        ctors[0].next = &ctors[1];

        ctors[1].name = "Point";
        ctors[1].params = NULL;
        ctors[1].next = NULL;

        sdecl.name = "Manifold";
        sdecl.type = TYPE_ADT;
        sdecl.adt.params = NULL;
        sdecl.adt.constraints = NULL;
        sdecl.adt.ctors = ctors;

        assert(ubik_value_new(&decl) == OK);
        assert(ubik_adt_create_decl(decl, &sdecl) == OK);

        assert(ubik_value_new(&args) == OK);
        args->tag |= TAG_LEFT_NODE | TAG_RIGHT_WORD;
        args->right.w = 0;
        assert(ubik_value_new(&args->left.t) == OK);
        assert(ubik_value_pack_string(args->left.t, "Point", 5) == OK);

        assert(ubik_value_new(&inst) == OK);
        assert(ubik_adt_instantiate(inst, decl, args) == OK);

        assert(ubik_adt_get_ctor(&res_str, inst) == OK);
        assert(strcmp(res_str, "Point") == 0);
        free(res_str);

        assert(ubik_adt_inst_size(&res_size, inst) == OK);
        assert(res_size == 0);

        free(ctors[0].params[0].type_expr);
        free(ctors[0].params);
        free(ctors);

        return ok;
}

run_single(simple_adt);
