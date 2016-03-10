/*
 * print-ast.c: AST printing methods
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

#include "expel/ast.h"

#include <inttypes.h>
#include <stdio.h>

no_ignore xl_error
_print_ast(struct xl_ast *ast, int indent);

static inline void
_indent(int indent)
{
        int i;
        for (i = 0; i < indent; i++)
                printf(" ");
}

no_ignore static xl_error
_print_atom(struct xl_ast_atom *atom)
{
        switch (atom->atom_type)
        {
        case ATOM_INT:
                printf("%" PRId64 ":i", (int64_t) atom->integer);
                return OK;
        case ATOM_NUM:
                printf("%f:f", atom->number);
                return OK;
        case ATOM_NAME:
                printf("%s:n", atom->str);
                return OK;
        case ATOM_QUALIFIED:
                printf("%s:%s:q", atom->qualified.head, atom->qualified.tail);
                return OK;
        case ATOM_TYPE_NAME:
                printf("%s:t", atom->str);
                return OK;
        case ATOM_STRING:
                printf("%s:s", atom->str);
                return OK;
        }

        return xl_raise(ERR_UNKNOWN_TYPE, "unknown atom type");
}

no_ignore static xl_error
_print_arg_list(struct xl_ast_arg_list *arg_list)
{
        printf("( ");
        while (arg_list->name != NULL)
        {
                printf("%s ", arg_list->name);
                arg_list = arg_list->next;
        }
        printf(")");
        return OK;
}

no_ignore static xl_error
_print_expr(struct xl_ast_expr *expr, int indent)
{
        xl_error err;

        switch (expr->expr_type)
        {
        case EXPR_ATOM:
                return _print_atom(expr->atom);

        case EXPR_APPLY:
                printf("(");
                err = _print_expr(expr->apply.head, indent);
                if (err != OK)
                        return err;
                printf(" ");
                err = _print_expr(expr->apply.tail, indent);
                if (err != OK)
                        return err;
                printf(")");
                return OK;

        case EXPR_LAMBDA:
                printf("\\ ");
                err = _print_arg_list(expr->lambda.args);
                if (err != OK)
                        return err;
                printf(" -> ");
                err = _print_expr(expr->lambda.body, indent);
                if (err != OK)
                        return err;
                return OK;

        case EXPR_CONSTRUCTOR:
                printf("%s {\n", expr->constructor.type_name);
                err = _print_ast(expr->constructor.scope, indent + 8);
                if (err != OK)
                        return err;
                _indent(indent + 4);
                printf("}");
                return OK;
        }

        return xl_raise(ERR_UNKNOWN_TYPE, "unknown expr type");
}

no_ignore static xl_error
_print_type_expr(struct xl_ast_type_expr *type_expr)
{
        xl_error err;

        switch (type_expr->type_expr_type)
        {
        case TYPE_EXPR_ATOM:
                printf("%s", type_expr->name);
                return OK;
        case TYPE_EXPR_APPLY:
                err = _print_type_expr(type_expr->apply.head);
                if (err != OK)
                        return err;
                printf(" -> (");
                err = _print_type_expr(type_expr->apply.tail);
                if (err != OK)
                        return err;
                printf(")");
                return OK;
        }
        return xl_raise(ERR_UNKNOWN_TYPE, "unknown type expr type");
}

no_ignore static xl_error
_print_type(struct xl_ast_type *type, int indent)
{
        struct xl_ast_member_list *m;
        xl_error err;

        _indent(indent);
        printf("type %s\n", type->name);

        switch (type->type)
        {
        case TYPE_RECORD:
                m = type->members;
                while (m != NULL)
                {
                        _indent(indent + 4);
                        printf(". %s ^ ", m->name);
                        err = _print_type_expr(m->type);
                        if (err != OK)
                                return err;
                        m = m->next;
                        printf("\n");
                }
        }

        return OK;
}

no_ignore xl_error
_print_ast(struct xl_ast *ast, int indent)
{
        size_t i;
        xl_error err;
        struct xl_ast_binding *b;

        if (ast->n_types > 0)
        {
                _indent(indent);
                printf("%lu types:\n", ast->n_types);
                for (i = 0; i < ast->n_types; i++)
                {
                        err = _print_type(ast->types[i], indent + 4);
                        if (err != OK)
                                return err;
                }
        }

        _indent(indent);
        printf("%lu bindings:\n", ast->n_bindings);
        for (i = 0; i < ast->n_bindings; i++)
        {
                b = ast->bindings[i];
                _indent(indent + 4);
                printf("bind %s", b->name);
                if (b->type_expr != NULL)
                {
                        printf(" ^ ");
                        err = _print_type_expr(b->type_expr);
                        if (err != OK)
                                return err;
                }
                printf(" = ");
                err = _print_expr(b->expr, indent);
                if (err != OK)
                        return err;
                printf("\n");
        }

        if (ast->immediate != NULL)
        {
                _indent(indent);
                printf("immediate = ");
                err = _print_expr(ast->immediate, indent);
                if (err != OK)
                        return err;
                printf("\n");
        }

        return OK;
}

no_ignore xl_error
xl_ast_print(struct xl_ast *ast)
{
        return _print_ast(ast, 0);
}