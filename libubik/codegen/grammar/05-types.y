/* vim: set shiftwidth=8 tabstop=8 :
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
: TYPE TYPE_NAME type_params type_constraints adt_ctors
{
        alloc($$, 1, struct ubik_type);
        $$->name = $2;
        $$->type = TYPE_ADT;
        $$->adt.params = $3;
        $$->adt.constraints = $4;
        $$->adt.ctors = $5;

        load_loc($$->loc);
        merge_loc($$, $$, $5);
}
;

type_params
: NAME type_params
{
        alloc($$, 1, struct ubik_type_params);
        $$->name = $1;
        $$->next = $2;
        load_loc($$->loc);
}
| %empty
{
        $$ = NULL;
}
;

type_constraints
: GIVEN EXISTS TYPE_NAME type_params type_constraints
{
        alloc($$, 1, struct ubik_type_constraints);
        $$->interface = $3;
        $$->params = $4;
        $$->next = $5;
        load_loc($$->loc);
        merge_loc($$, $$, $4);
}
| %empty
{
        $$ = NULL;
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
: type_expr
| type_expr GOES_TO top_type_expr
{
        alloc($$, 1, struct ubik_type_expr);
        $$->type_expr_type = TYPE_EXPR_ARROW;
        $$->apply.head = $1;
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
        $$->name = $1;
        load_loc($$->loc);
}
| TYPE_NAME
{
        alloc($$, 1, struct ubik_type_expr);
        $$->type_expr_type = TYPE_EXPR_ATOM;
        $$->name = $1;
        load_loc($$->loc);
}
| QUALIFIED_TYPE_NAME
{
        alloc($$, 1, struct ubik_type_expr);
        $$->type_expr_type = TYPE_EXPR_ATOM;
        $$->name = $1;
        load_loc($$->loc);
}
;
