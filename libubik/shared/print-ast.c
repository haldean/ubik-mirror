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

#include "ubik/ast.h"
#include "ubik/resolve.h"

#include <inttypes.h>
#include <stdio.h>
#include <string.h>

no_ignore ubik_error
_print_ast(struct ubik_ast *ast, int indent);

no_ignore static ubik_error
_print_expr(struct ubik_ast_expr *expr, int indent);

static inline void
_indent(int indent)
{
        int i;
        for (i = 0; i < indent; i++)
                printf(" ");
}

no_ignore static ubik_error
_print_atom(struct ubik_ast_atom *atom)
{
        size_t i;
        size_t n;

        if (atom->name_loc != 0)
        {
                switch (atom->name_loc->type)
                {
                case RESOLVE_LOCAL:
                        printf(".");
                        break;
                case RESOLVE_GLOBAL:
                        printf("*");
                        break;
                case RESOLVE_NATIVE:
                        printf("@");
                        break;
                case RESOLVE_CLOSURE:
                        printf("%%");
                        break;
                }
        }
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
                n = strlen(atom->str);
                putchar('"');
                for (i = 0; i < n; i++)
                {
                        if (atom->str[i] == '\n')
                                printf("\\n");
                        else if (atom->str[i] == '\t')
                                printf("\\t");
                        else if (atom->str[i] == '\r')
                                printf("\\r");
                        else
                                putchar(atom->str[i]);
                }
                printf("\":s");
                return OK;
        }

        return ubik_raise(ERR_UNKNOWN_TYPE, "unknown atom type");
}

no_ignore static ubik_error
_print_arg_list(struct ubik_ast_arg_list *arg_list)
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

no_ignore static ubik_error
_print_case_stmts(struct ubik_ast_case *case_stmt, int indent)
{
        ubik_error err;

        while (case_stmt != NULL)
        {
                _indent(indent);
                printf("( ");
                if (case_stmt->head != NULL)
                {
                        err = _print_expr(case_stmt->head, indent);
                        if (err != OK)
                                return err;
                }
                printf(" => ");
                err = _print_expr(case_stmt->tail, indent);
                if (err != OK)
                        return err;
                printf(" )\n");

                case_stmt = case_stmt->next;
        }
        return OK;
}

no_ignore static ubik_error
_print_expr(struct ubik_ast_expr *expr, int indent)
{
        ubik_error err;

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

        case EXPR_BLOCK:
                printf("{\n");
                err = _print_ast(expr->block, indent + 8);
                if (err != OK)
                        return err;
                _indent(indent + 4);
                printf("}");
                return OK;

        case EXPR_COND_BLOCK:
                printf("? ");
                if (expr->cond_block.block_type == COND_PATTERN)
                {
                        err = _print_expr(
                                expr->cond_block.to_match, indent + 4);
                        if (err != OK)
                                return err;
                        printf(" ");
                }
                printf("{\n");
                err = _print_case_stmts(
                        expr->cond_block.case_stmts, indent + 4);
                if (err != OK)
                        return err;
                printf("}");
                return OK;
        }

        return ubik_raise(ERR_UNKNOWN_TYPE, "unknown expr type");
}

no_ignore static ubik_error
_print_type_expr(struct ubik_ast_type_expr *type_expr)
{
        ubik_error err;

        switch (type_expr->type_expr_type)
        {
        case TYPE_EXPR_ATOM:
                printf("%s", type_expr->name);
                return OK;
        case TYPE_EXPR_ARROW:
                err = _print_type_expr(type_expr->apply.head);
                if (err != OK)
                        return err;
                printf(" -> (");
                err = _print_type_expr(type_expr->apply.tail);
                if (err != OK)
                        return err;
                printf(")");
                return OK;
        case TYPE_EXPR_APPLY:
                err = _print_type_expr(type_expr->apply.head);
                if (err != OK)
                        return err;
                printf(" (");
                err = _print_type_expr(type_expr->apply.tail);
                if (err != OK)
                        return err;
                printf(")");
                return OK;
        case TYPE_EXPR_VAR:
                printf("%s", type_expr->name);
                return OK;
        }
        return ubik_raise(ERR_UNKNOWN_TYPE, "unknown type expr type");
}

no_ignore static ubik_error
_print_type_list(struct ubik_ast_type_list *t)
{
        ubik_error err;

        while (t != NULL)
        {
                err = _print_type_expr(t->type_expr);
                if (err != OK)
                        return err;
                t = t->next;
                if (t != NULL)
                        printf(" ");
        }

        return OK;
}

no_ignore static ubik_error
_print_type(struct ubik_ast_type *type, int indent)
{
        struct ubik_ast_member_list *m;
        struct ubik_ast_type_params *p;
        struct ubik_ast_type_constraints *c;
        struct ubik_ast_adt_ctors *ctor;
        ubik_error err;

        _indent(indent);
        printf("type %s", type->name);

        switch (type->type)
        {
        case TYPE_RECORD:
                printf("\n");
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
                break;

        case TYPE_ADT:
                p = type->adt.params;
                while (p != NULL)
                {
                        printf(" %s", p->name);
                        p = p->next;
                        if (p != NULL)
                                printf(" ");
                }
                printf("\n");

                _indent(indent + 4);
                printf("|");
                c = type->adt.constraints;
                while (c != NULL)
                {
                        printf(" ' %s", c->interface);
                        p = c->params;
                        while (p != NULL)
                        {
                                printf(" %s", p->name);
                                p = p->next;
                                if (p != NULL)
                                        printf(" ");
                        }
                        c = c->next;
                }
                printf("\n");

                ctor = type->adt.ctors;
                while (ctor != NULL)
                {
                        _indent(indent + 4);
                        printf("= %s ", ctor->name);
                        err = _print_type_list(ctor->params);
                        if (err != OK)
                                return err;
                        ctor = ctor->next;
                        printf("\n");
                }

                break;

        case TYPE_ALIAS:
                printf(" = ");
                err = _print_type_expr(type->aliases_to);
                if (err != OK)
                        return err;
                printf("\n");
        }

        return OK;
}

no_ignore ubik_error
_print_ast(struct ubik_ast *ast, int indent)
{
        size_t i;
        ubik_error err;
        struct ubik_ast_binding *b;

        if (ast->types.n > 0)
        {
                _indent(indent);
                printf("%lu types:\n", ast->types.n);
                for (i = 0; i < ast->types.n; i++)
                {
                        err = _print_type(ast->types.elems[i], indent + 4);
                        if (err != OK)
                                return err;
                }
        }

        _indent(indent);
        printf("%lu bindings:\n", ast->bindings.n);
        for (i = 0; i < ast->bindings.n; i++)
        {
                b = ast->bindings.elems[i];
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

no_ignore ubik_error
ubik_ast_print(struct ubik_ast *ast)
{
        return _print_ast(ast, 0);
}
