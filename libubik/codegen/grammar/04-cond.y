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

cond_block
: COND expr OPEN_SCOPE all_pattern_case_stmts CLOSE_SCOPE
{
        alloc($$, 1, struct ubik_ast_expr);
        $$->expr_type = EXPR_COND_BLOCK;
        $$->cond_block.block_type = COND_PATTERN;
        $$->cond_block.to_match = $2;
        $$->cond_block.case_stmts = $4;
        load_loc($$->loc);
        merge_loc($$, $$, $4);
}
| COND OPEN_SCOPE all_pred_case_stmts CLOSE_SCOPE
{
        alloc($$, 1, struct ubik_ast_expr);
        $$->expr_type = EXPR_COND_BLOCK;
        $$->cond_block.block_type = COND_PREDICATE;
        $$->cond_block.to_match = NULL;
        $$->cond_block.case_stmts = $3;
        load_loc($$->loc);
        merge_loc($$, $$, $3);
}
;


all_pattern_case_stmts
: all_pattern_case_stmts pattern_case_stmt
{
        $$ = $1;
        while ($1->next != NULL)
                $1 = $1->next;
        $1->next = $2;
}
| pattern_case_stmt
;

pattern_case_stmt
: MEMBER pattern IMPLIES expr
{
        alloc($$, 1, struct ubik_ast_case);
        $$->head = $2;
        $$->tail = $4;
        load_loc($$->loc);
        merge_loc($$, $$, $4);
}

all_pred_case_stmts
: pred_case_stmts last_pred_case_stmt
{
        $$ = $1;
        while ($1->next != NULL)
                $1 = $1->next;
        $1->next = $2;
}
| pred_case_stmts
;

pred_case_stmts
: pred_case_stmts pred_case_stmt
{
        $$ = $1;
        while ($1->next != NULL)
                $1 = $1->next;
        $1->next = $2;
}
| pred_case_stmt
;

last_pred_case_stmt
: MEMBER IMPLIES expr
{
        alloc($$, 1, struct ubik_ast_case);
        $$->head = NULL;
        $$->tail = $3;
        load_loc($$->loc);
        merge_loc($$, $$, $3);
}
;

pred_case_stmt
: MEMBER expr IMPLIES expr
{
        alloc($$, 1, struct ubik_ast_case);
        $$->head = $2;
        $$->tail = $4;
        load_loc($$->loc);
        merge_loc($$, $$, $4);
}
;

pattern
: pattern NAME
{
        struct ubik_ast_expr *tail;
        struct ubik_ast_atom *atom;

        alloc(atom, 1, struct ubik_ast_atom);
        atom->atom_type = ATOM_NAME;
        atom->str = $2;
        load_loc(atom->loc);

        alloc(tail, 1, struct ubik_ast_expr);
        tail->expr_type = EXPR_ATOM;
        tail->atom = atom;
        tail->loc = atom->loc;

        alloc($$, 1, struct ubik_ast_expr);
        $$->expr_type = EXPR_APPLY;
        $$->apply.head = $1;
        $$->apply.tail = tail;

        merge_loc($$, $1, tail);
}
| TYPE_NAME
{
        struct ubik_ast_atom *atom;
        alloc(atom, 1, struct ubik_ast_atom);
        atom->atom_type = ATOM_TYPE_NAME;
        atom->str = $1;
        load_loc(atom->loc);

        alloc($$, 1, struct ubik_ast_expr);
        $$->expr_type = EXPR_ATOM;
        $$->atom = atom;
        $$->loc = atom->loc;
}
| QUALIFIED_TYPE_NAME
{
        struct ubik_ast_atom *atom;
        wrap_err(ubik_ast_atom_new_qualified(&atom, $1, ctx->region));
        load_loc(atom->loc);

        alloc($$, 1, struct ubik_ast_expr);
        $$->expr_type = EXPR_ATOM;
        $$->atom = atom;
        $$->loc = atom->loc;
}
;

