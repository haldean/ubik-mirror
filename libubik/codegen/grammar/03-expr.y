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

binding
: BIND NAME IS top_expr
{
        alloc($$, 1, struct ubik_ast_binding);
        $$->name = $2;
        $$->expr = $4;

        load_loc($$->loc);
        merge_loc($$, $$, $4);
}
| BIND NAME TYPE top_type_expr IS top_expr
{
        alloc($$, 1, struct ubik_ast_binding);
        $$->name = $2;
        $$->expr = $6;
        $$->type_expr = $4;

        load_loc($$->loc);
        merge_loc($$, $$, $4);
        merge_loc($$, $$, $6);
}
;

/* top_expr is a "top expression" in the parse tree; these are things that can't
* be subexpressions without first being wrapped in parentheses. Without the
* parentheses around a top expression inside another expression, the grammar is
* ambiguous. */
top_expr
: expr
| LAMBDA arg_list GOES_TO top_expr
{
        alloc($$, 1, struct ubik_ast_expr);
        $$->expr_type = EXPR_LAMBDA;
        $$->lambda.args = $2;
        $$->lambda.body = $4;

        load_loc($$->loc);
        if ($2 != NULL)
                merge_loc($$, $$, $2);
        merge_loc($$, $$, $4);
}
| cond_block
;

expr
: expr atom
{
        struct ubik_ast_expr *tail;
        alloc(tail, 1, struct ubik_ast_expr);
        tail->expr_type = EXPR_ATOM;
        tail->atom = $2;
        tail->loc = $2->loc;

        alloc($$, 1, struct ubik_ast_expr);
        $$->expr_type = EXPR_APPLY;
        $$->apply.head = $1;
        $$->apply.tail = tail;

        merge_loc($$, $1, $2);
}
| expr OPEN_PAR top_expr CLOSE_PAR
{
        alloc($$, 1, struct ubik_ast_expr);
        $$->expr_type = EXPR_APPLY;
        $$->apply.head = $1;
        $$->apply.tail = $3;
        merge_loc($$, $1, $3);
}
| atom
{
        alloc($$, 1, struct ubik_ast_expr);
        $$->expr_type = EXPR_ATOM;
        $$->atom = $1;
        $$->loc = $1->loc;
}
| OPEN_PAR top_expr CLOSE_PAR
{
        $$ = $2;
}
| OPEN_SCOPE blocks CLOSE_SCOPE
{
        alloc($$, 1, struct ubik_ast_expr);
        $$->expr_type = EXPR_BLOCK;
        $$->block = $2;
        load_loc($$->loc);
        merge_loc($$, $$, $2);
}
;

arg_list
: %empty
{
        $$ = NULL;
}
| NAME arg_list
{
        alloc($$, 1, struct ubik_ast_arg_list);
        $$->name = $1;
        $$->next = $2;
        load_loc($$->loc);
}
;

atom
: NAME
{
        alloc($$, 1, struct ubik_ast_atom);
        $$->atom_type = ATOM_NAME;
        $$->str = $1;
        load_loc($$->loc);
}
| QUALIFIED_NAME
{
        wrap_err(ubik_ast_atom_new_qualified(&$$, $1, ctx->region));
        load_loc($$->loc);
}
| TYPE_NAME
{
        alloc($$, 1, struct ubik_ast_atom);
        $$->atom_type = ATOM_TYPE_NAME;
        $$->str = $1;
        load_loc($$->loc);
}
| QUALIFIED_TYPE_NAME
{
        wrap_err(ubik_ast_atom_new_qualified(&$$, $1, ctx->region));
        load_loc($$->loc);
}
| INTEGER
{
        alloc($$, 1, struct ubik_ast_atom);
        $$->atom_type = ATOM_INT;
        $$->integer = $1;
        load_loc($$->loc);
}
| NUMBER
{
        alloc($$, 1, struct ubik_ast_atom);
        $$->atom_type = ATOM_NUM;
        wrap_err(ubik_rat_read(&$$->number, $1));
        load_loc($$->loc);
}
| STRING
{
        alloc($$, 1, struct ubik_ast_atom);
        $$->atom_type = ATOM_STRING;
        $$->str = $1;
        load_loc($$->loc);
}
;
