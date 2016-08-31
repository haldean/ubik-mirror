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

top_result
: MATCH_PROG prog
{
        $$.prog = $2;
}
| MATCH_TYPE top_type_expr
{
        $$.type_expr = $2;
        ctx->type_expr = $2;
}
;

prog
: prog package
{
        $1->package_name = $2;
        $$ = $1;
}
| prog binding
{
        wrap_err(ubik_vector_append(&$1->bindings, $2));
        $$ = $1;
        merge_loc($$, $$, $2);
}
| prog type_def
{
        wrap_err(ubik_vector_append(&$1->types, $2));
        $$ = $1;
        merge_loc($$, $$, $2);
}
| prog import
{
        $2->next = $1->imports;
        $1->imports = $2;
        $$ = $1;
        merge_loc($$, $$, $2);
}
| prog immediate
{
        $1->immediate = $2;
        $$ = $1;
        merge_loc($$, $$, $2);
}
| prog interface_def
{
        wrap_err(ubik_vector_append(&$1->interfaces, $2));
        $$ = $1;
        merge_loc($$, $$, $2);
}
| prog impl_def
{
        wrap_err(ubik_vector_append(&$1->implementations, $2));
        $$ = $1;
        merge_loc($$, $$, $2);
}
| %empty
{
        ubik_ast_new(&$$, ctx->region);
        load_loc($$->loc);
        ctx->ast = $$;
}
;

/* blocks is a list of bindings with a single optional immediate statement
 * within */
blocks
: binding
{
        ubik_ast_new(&$$, ctx->region);
        load_loc($$->loc);
        wrap_err(ubik_vector_append(&$$->bindings, $1));
        merge_loc($$, $$, $1);
}
| immediate
{
        ubik_ast_new(&$$, ctx->region);
        load_loc($$->loc);
        $$->immediate = $1;
        merge_loc($$, $$, $1);
}
| binding blocks
{
        wrap_err(ubik_vector_append(&$2->bindings, $1));
        $$ = $2;
        merge_loc($$, $$, $1);
}
| immediate bindings
{
        $2->immediate = $1;
        $$ = $2;
        merge_loc($$, $$, $1);
}
;

bindings
: binding
{
        ubik_ast_new(&$$, ctx->region);
        wrap_err(ubik_vector_append(&$$->bindings, $1));
        load_loc($$->loc);
}
| bindings binding
{
        wrap_err(ubik_vector_append(&$1->bindings, $2));
        $$ = $1;
        merge_loc($$, $$, $2);
}

immediate
: IMMEDIATE top_expr
{
        $$ = $2;
        load_loc($$->loc);
        merge_loc($$, $$, $2);
}
;
