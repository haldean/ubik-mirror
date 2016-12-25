/*
 * ast.c: in-memory ast representation
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

#include "ubik/assert.h"
#include "ubik/ast.h"
#include "ubik/string.h"

#include <stdlib.h>
#include <string.h>

no_ignore ubik_error
ubik_ast_atom_new_qualified(
        struct ubik_ast_atom **atom,
        char *name,
        struct ubik_alloc_region *r)
{
        size_t head_len;
        size_t tail_len;
        size_t name_len;
        size_t i;

        head_len = 0;
        tail_len = 0;
        name_len = strlen(name);
        for (i = 0; i < name_len; i++)
        {
                if (name[i] == ':')
                {
                        head_len = i;
                        tail_len = name_len - i - 1;
                        break;
                }
        }

        ubik_assert(head_len > 0);
        ubik_assert(tail_len > 0);

        ubik_alloc1(atom, struct ubik_ast_atom, r);
        (*atom)->atom_type = ATOM_QUALIFIED;

        ubik_ralloc(
                (void **) &(*atom)->qualified.head,
                head_len + 1, sizeof(char), r);
        memcpy((*atom)->qualified.head, name, head_len);

        ubik_ralloc(
                (void **) &(*atom)->qualified.tail,
                tail_len + 1, sizeof(char), r);
        memcpy((*atom)->qualified.tail, &name[head_len + 1], tail_len);

        return OK;
}

no_ignore ubik_error
ubik_ast_import(
        struct ubik_ast *ast,
        struct ubik_ast_import_list *import_list)
{
        import_list->next = ast->imports;
        ast->imports = import_list;
        return OK;
}

no_ignore ubik_error
ubik_ast_subexprs(
        struct ubik_ast **subast,
        struct ubik_ast_expr **subexprs,
        size_t *n_subexprs,
        struct ubik_ast_expr *expr)
{
        struct ubik_ast_case *case_stmt;
        size_t i;

        if (subast != NULL)
                *subast = NULL;
        if (n_subexprs != NULL)
                *n_subexprs = 0;

        switch (expr->expr_type)
        {
        case EXPR_ATOM:
                return OK;

        case EXPR_APPLY:
                if (subexprs == NULL)
                        return OK;

                subexprs[0] = expr->apply.head;
                subexprs[1] = expr->apply.tail;
                *n_subexprs = 2;
                return OK;

        case EXPR_LAMBDA:
                if (subexprs == NULL)
                        return OK;

                subexprs[0] = expr->lambda.body;
                *n_subexprs = 1;
                return OK;

        case EXPR_COND_BLOCK:
                if (subexprs == NULL)
                        return OK;

                i = 0;
                switch (expr->cond_block.block_type)
                {
                case COND_PATTERN:
                        subexprs[i++] = expr->cond_block.to_match;
                        break;
                case COND_PREDICATE:
                        break;
                }

                case_stmt = expr->cond_block.case_stmts;
                while (case_stmt != NULL)
                {
                        if (i >= UBIK_MAX_SUBEXPRS - 1)
                                return ubik_raise(
                                        ERR_BAD_VALUE,
                                        "too many case statements");
                        /* We don't consider patterns expressions, because they
                         * often need to be treated in a very different way from
                         * a normal expression. */
                        if (case_stmt->head != NULL
                                && expr->cond_block.block_type != COND_PATTERN)
                        {
                                subexprs[i++] = case_stmt->head;
                        }
                        subexprs[i++] = case_stmt->tail;
                        case_stmt = case_stmt->next;
                }
                *n_subexprs = i;
                return OK;

        case EXPR_BLOCK:
                if (subast == NULL)
                        return OK;
                *subast = expr->block;
                return OK;
        }

        return ubik_raise(ERR_BAD_TYPE, "bad type in expr subexpressions");
}

void
ubik_ast_merge_loc(
        struct ubik_ast_loc *res,
        struct ubik_ast_loc *l1,
        struct ubik_ast_loc *l2)
{
        res->line_start = size_min(l1->line_start, l2->line_start);
        res->line_end = size_max(l1->line_end, l2->line_end);
        res->col_start = size_min(l1->col_start, l2->col_start);
        res->col_end = size_max(l1->col_end, l2->col_end);
        res->source_name = l1->source_name;
        res->source = l1->source;
}

void
ubik_ast_new(struct ubik_ast **ast, struct ubik_alloc_region *region)
{
        ubik_alloc1(ast, struct ubik_ast, region);
        (*ast)->bindings.region = region;
        (*ast)->types.region = region;
        (*ast)->interfaces.region = region;
        (*ast)->implementations.region = region;
        (*ast)->imported_bindings.region = region;
        (*ast)->tests.region = region;
}
