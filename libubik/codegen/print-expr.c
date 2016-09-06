/*
 * print-expr.c: pretty-printing for expressions
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
#include "ubik/stream.h"
#include "ubik/string.h"
#include "ubik/types.h"
#include "ubik/util.h"

#include <inttypes.h>
#include <string.h>

#define indent(i) do { \
                for (int __i = 0; __i < (i); __i++) ubik_fprintf(out, " "); \
        } while (0)

static void
apply_pretty(
        struct ubik_stream *out,
        struct ubik_ast_expr *expr,
        int start_indent)
{
        switch (expr->apply.head->expr_type)
        {
        case EXPR_ATOM:
        case EXPR_APPLY:
        case EXPR_BLOCK:
                ubik_ast_expr_pretty(out, expr->apply.head, start_indent);
                break;

        case EXPR_COND_BLOCK:
        case EXPR_LAMBDA:
                ubik_fprintf(out, "(");
                ubik_ast_expr_pretty(out, expr->apply.head, start_indent);
                ubik_fprintf(out, ")");
                break;
        }

        switch (expr->apply.tail->expr_type)
        {
        case EXPR_ATOM:
        case EXPR_BLOCK:
                ubik_fprintf(out, " ");
                ubik_ast_expr_pretty(out, expr->apply.tail, start_indent);
                break;

        case EXPR_COND_BLOCK:
        case EXPR_APPLY:
        case EXPR_LAMBDA:
                ubik_fprintf(out, " (");
                ubik_ast_expr_pretty(out, expr->apply.tail, start_indent);
                ubik_fprintf(out, ")");
                break;
        }
}

static void
atom_pretty(
        struct ubik_stream *out,
        struct ubik_ast_atom *atom)
{
        size_t i, n;

        switch (atom->atom_type)
        {
        case ATOM_INT:
                ubik_fprintf(out, "%" PRIu64, atom->integer);
                return;
        case ATOM_NUM:
                ubik_fprintf(out, "%lf", atom->number);
                return;
        case ATOM_NAME:
                ubik_fprintf(out, "%s", atom->str);
                return;
        case ATOM_QUALIFIED:
                ubik_fprintf(
                        out, "%s:%s", atom->qualified.head,
                        atom->qualified.tail);
                return;
        case ATOM_TYPE_NAME:
                ubik_fprintf(out, "%s", atom->str);
                return;
        case ATOM_STRING:
                n = strlen(atom->str);
                ubik_fprintf(out, "\"");
                for (i = 0; i < n; i++)
                {
                        switch (atom->str[i])
                        {
                        case '\r':
                                ubik_fprintf(out, "\\r");
                                break;
                        case '\n':
                                ubik_fprintf(out, "\\n");
                                break;
                        case '\t':
                                ubik_fprintf(out, "\\t");
                                break;
                        default:
                                ubik_assert(ubik_stream_write(
                                        out, &atom->str[i], 1) == 1);

                                break;
                        }
                }
                ubik_fprintf(out, "\"");
                return;
        case ATOM_VALUE:
                ubik_fprintf(out, "(internal ubik state)");
                return;
        }
}

static void
lambda_pretty(
        struct ubik_stream *out,
        struct ubik_ast_expr *expr,
        int start_indent)
{
        struct ubik_ast_arg_list *args;
        ubik_fprintf(out, "\u03bb");
        for (args = expr->lambda.args; args != NULL; args = args->next)
                ubik_fprintf(out, " %s", args->name);
        ubik_fprintf(out, " -> ");
        ubik_ast_expr_pretty(out, expr->lambda.body, start_indent);
}

static void
block_pretty(
        struct ubik_stream *out,
        struct ubik_ast_expr *expr,
        int start_indent)
{
        struct ubik_ast_binding *bind;
        size_t i;
        int in;

        in = start_indent + 4;
        ubik_fprintf(out, "{\n");

        for (i = 0; i < expr->block->bindings.n; i++)
        {
                bind = expr->block->bindings.elems[i];
                indent(in);
                ubik_fprintf(out, ": %s", bind->name);
                if (bind->type_expr != NULL)
                {
                        ubik_fprintf(out, " ^ ");
                        ubik_type_expr_pretty(out, bind->type_expr);
                }
                ubik_fprintf(out, " = ");
                ubik_ast_expr_pretty(out, bind->expr, in);
                ubik_fprintf(out, "\n");
        }

        if (expr->block->immediate != NULL)
        {
                indent(in);
                ubik_fprintf(out, "! ");
                ubik_ast_expr_pretty(out, expr->block->immediate, in);
                ubik_fprintf(out, "\n");
        }

        indent(start_indent);
        ubik_fprintf(out, "}");
}

static void
cond_block_pretty(
        struct ubik_stream *out,
        struct ubik_ast_expr *expr,
        int start_indent)
{
        struct ubik_ast_case *case_stmt;

        ubik_fprintf(out, "? ");
        if (expr->cond_block.block_type == COND_PATTERN)
        {
                switch (expr->cond_block.to_match->expr_type)
                {
                case EXPR_ATOM:
                case EXPR_APPLY:
                        ubik_ast_expr_pretty(
                                out, expr->cond_block.to_match, start_indent);
                        ubik_fprintf(out, " ");
                        break;

                case EXPR_LAMBDA:
                case EXPR_COND_BLOCK:
                case EXPR_BLOCK:
                        ubik_fprintf(out, "(");
                        ubik_ast_expr_pretty(
                                out, expr->cond_block.to_match, start_indent);
                        ubik_fprintf(out, ") ");
                        break;
                }
        }
        ubik_fprintf(out, "{\n");

        for (case_stmt = expr->cond_block.case_stmts;
             case_stmt != NULL;
             case_stmt = case_stmt->next)
        {
                indent(start_indent + 4);
                ubik_fprintf(out, ". ");
                ubik_ast_expr_pretty(out, case_stmt->head, start_indent + 4);
                ubik_fprintf(out, " => ");
                ubik_ast_expr_pretty(out, case_stmt->tail, start_indent + 4);
                ubik_fprintf(out, "\n");
        }

        indent(start_indent);
        ubik_fprintf(out, "}");
}

void
ubik_ast_expr_pretty(
        struct ubik_stream *out,
        struct ubik_ast_expr *expr,
        int start_indent)
{
        switch (expr->expr_type)
        {
        case EXPR_APPLY:
                apply_pretty(out, expr, start_indent);
                return;

        case EXPR_ATOM:
                atom_pretty(out, expr->atom);
                return;

        case EXPR_LAMBDA:
                lambda_pretty(out, expr, start_indent);
                return;

        case EXPR_BLOCK:
                block_pretty(out, expr, start_indent);
                return;

        case EXPR_COND_BLOCK:
                cond_block_pretty(out, expr, start_indent);
                return;
        }

        ubik_fprintf(out, "(unknown expression type)");
}
