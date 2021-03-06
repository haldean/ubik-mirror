/* vim: set shiftwidth=8 tabstop=8 filetype=text :
 * grammar.y: ubik language grammar
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

type_def
: adt_def
| alias_def
;

/* Type aliases */
alias_def
: BIND TYPE_NAME IS top_type_expr
{
        alloc($$, 1, struct ubik_type);
        $$->name = $2;
        $$->type = TYPE_ALIAS;
        $$->aliases_to = $4;

        load_loc($$->loc);
        merge_loc($$, $$, $4);
}

/* Abstract Data Types */
adt_def
: TYPE TYPE_NAME type_params GIVEN type_constraints adt_ctors
{
        alloc($$, 1, struct ubik_type);
        $$->name = $2;
        $$->type = TYPE_ADT;
        $$->adt.params = $3;
        $$->adt.constraints = $5;
        $$->adt.ctors = $6;

        load_loc($$->loc);
        merge_loc($$, $$, $6);
}
| TYPE TYPE_NAME type_params adt_ctors
{
        alloc($$, 1, struct ubik_type);
        $$->name = $2;
        $$->type = TYPE_ADT;
        $$->adt.params = $3;
        $$->adt.constraints = NULL;
        $$->adt.ctors = $4;

        load_loc($$->loc);
        merge_loc($$, $$, $4);
}
;

type_params
: NAME type_params
{
        alloc($$, 1, struct ubik_type_params);
        $$->name.name = $1;
        $$->name.package = NULL;
        $$->next = $2;
        load_loc($$->loc);
}
| QUALIFIED_NAME type_params
{
        alloc($$, 1, struct ubik_type_params);
        wrap_err(ubik_ast_read_qualified(
                &$$->name.package, &$$->name.name, $1, ctx->region));
        $$->next = $2;
        load_loc($$->loc);
}
| %empty { $$ = NULL; }
;

nonempty_type_params
: NAME type_params
{
        alloc($$, 1, struct ubik_type_params);
        $$->name.name = $1;
        $$->name.package = NULL;
        $$->next = $2;
        load_loc($$->loc);
};
| QUALIFIED_NAME type_params
{
        alloc($$, 1, struct ubik_type_params);
        wrap_err(ubik_ast_read_qualified(
                &$$->name.package, &$$->name.name, $1, ctx->region));
        $$->next = $2;
        load_loc($$->loc);
};

type_constraints
: type_constraint type_constraints
{
        $$ = $1;
        $$->next = $2;
}
| type_constraint
;

type_constraint
: EXISTS TYPE_NAME nonempty_type_params
{
        alloc($$, 1, struct ubik_type_constraints);
        $$->interface.name = $2;
        $$->interface.package = NULL;
        $$->params = $3;
        $$->next = NULL;
        load_loc($$->loc);
        merge_loc($$, $$, $3);
}
| EXISTS QUALIFIED_TYPE_NAME nonempty_type_params
{
        alloc($$, 1, struct ubik_type_constraints);
        wrap_err(ubik_ast_read_qualified(
                &$$->interface.package, &$$->interface.name, $2, ctx->region));
        $$->params = $3;
        $$->next = NULL;
        load_loc($$->loc);
        merge_loc($$, $$, $3);
}
;

adt_ctors
: adt_ctor adt_ctors
{
        $$ = $1;
        $$->next = $2;
}
| adt_ctor
;

adt_ctor
: IS TYPE_NAME type_list
{
        alloc($$, 1, struct ubik_ast_adt_ctors);
        $$->name = $2;
        $$->params = $3;
        load_loc($$->loc);
        if ($3 != NULL)
                merge_loc($$, $$, $3);
}
;

type_list
: type_atom type_list
{
        alloc($$, 1, struct ubik_type_list);
        $$->type_expr = $1;
        $$->next = $2;
        load_loc($$->loc);
}
| OPEN_PAR top_type_expr CLOSE_PAR type_list
{
        alloc($$, 1, struct ubik_type_list);
        $$->type_expr = $2;
        $$->next = $4;
        load_loc($$->loc);
}
| %empty
{
        $$ = NULL;
}
;

top_type_expr
: type_apply_expr
| type_apply_expr GIVEN type_constraints
{
        alloc($$, 1, struct ubik_type_expr);
        $$->type_expr_type = TYPE_EXPR_CONSTRAINED;
        $$->constrained.term = $1;
        $$->constrained.constraints = $3;
        merge_loc($$, $1, $3);
}
;

type_apply_expr
: type_expr
| type_expr GOES_TO type_apply_expr
{
        /* The arrow notation A -> B is just convenient syntax for Function a b;
        this desugars at compile time */
        alloc($$, 1, struct ubik_type_expr);
        $$->type_expr_type = TYPE_EXPR_APPLY;

        alloc($$->apply.head, 1, struct ubik_type_expr);
        $$->apply.head->type_expr_type = TYPE_EXPR_APPLY;

        alloc($$->apply.head->apply.head, 1, struct ubik_type_expr);
        $$->apply.head->apply.head->type_expr_type = TYPE_EXPR_ATOM;
        $$->apply.head->apply.head->name.name = ubik_strdup(
                UBIK_FUNCTION_CONSTRUCTOR, ctx->region);
        $$->apply.head->apply.head->name.package = ubik_strdup(
                UBIK_PACKAGE, ctx->region);

        $$->apply.head->apply.tail = $1;
        $$->apply.tail = $3;
        merge_loc($$, $1, $3);
}
;

type_expr
: type_expr type_atom
{
        alloc($$, 1, struct ubik_type_expr);
        $$->type_expr_type = TYPE_EXPR_APPLY;
        $$->apply.head = $1;
        $$->apply.tail = $2;
        merge_loc($$, $1, $2);
}
| type_expr OPEN_PAR top_type_expr CLOSE_PAR
{
        alloc($$, 1, struct ubik_type_expr);
        $$->type_expr_type = TYPE_EXPR_APPLY;
        $$->apply.head = $1;
        $$->apply.tail = $3;
        merge_loc($$, $1, $3);
}
| type_atom
| OPEN_PAR top_type_expr CLOSE_PAR
{
        $$ = $2;
}
;

type_atom
: NAME
{
        alloc($$, 1, struct ubik_type_expr);
        $$->type_expr_type = TYPE_EXPR_VAR;
        $$->name.name = $1;
        $$->name.package = NULL;
        load_loc($$->loc);
}
| TYPE_NAME
{
        alloc($$, 1, struct ubik_type_expr);
        $$->type_expr_type = TYPE_EXPR_ATOM;
        $$->name.name = $1;
        $$->name.package = NULL;
        load_loc($$->loc);
}
| QUALIFIED_NAME
{
        alloc($$, 1, struct ubik_type_expr);
        $$->type_expr_type = TYPE_EXPR_VAR;
        wrap_err(ubik_ast_read_qualified(
                &$$->name.package, &$$->name.name, $1, ctx->region));
        load_loc($$->loc);
}
| QUALIFIED_TYPE_NAME
{
        alloc($$, 1, struct ubik_type_expr);
        $$->type_expr_type = TYPE_EXPR_ATOM;
        wrap_err(ubik_ast_read_qualified(
                &$$->name.package, &$$->name.name, $1, ctx->region));
        load_loc($$->loc);
}
;
