/*
 * ast.h: in-memory ast representation
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
#include <stdio.h>
#include <stdlib.h>

#define check_alloc(x, nelem, contents) { \
        (x) = calloc(nelem, sizeof(contents)); \
        if ((x) == NULL) return xl_raise(ERR_NO_MEMORY, ""); }

/* Allocates a new AST. */
no_ignore xl_error
xl_ast_new(struct xl_ast **ast)
{
        check_alloc(*ast, 1, struct xl_ast);
        return OK;
}

no_ignore static xl_error
_free_atom(struct xl_ast_atom *atom)
{
        switch (atom->atom_type)
        {
        case ATOM_NAME:
        case ATOM_TYPE_NAME:
        case ATOM_STRING:
                free(atom->str);
                break;

        case ATOM_NUM:
        case ATOM_INT:
                break;

        default:
                return xl_raise(ERR_BAD_TYPE, "unknown atom type in free");
        }

        free(atom);
        return OK;
}

no_ignore static xl_error
_free_arg_list(struct xl_ast_arg_list *arg_list)
{
        struct xl_ast_arg_list *next;

        while (arg_list != NULL)
        {
                next = arg_list->next;
                if (arg_list->name != NULL)
                        free(arg_list->name);
                free(arg_list);
                arg_list = next;
        }
        return OK;
}

no_ignore static xl_error
_free_expr(struct xl_ast_expr *expr)
{
        xl_error err;

        switch (expr->expr_type)
        {
        case EXPR_ATOM:
                err = _free_atom(expr->atom);
                break;
        case EXPR_APPLY:
                err = _free_expr(expr->apply.head);
                if (err != OK)
                        return err;
                err = _free_expr(expr->apply.tail);
                break;
        case EXPR_LAMBDA:
                err = _free_expr(expr->lambda.body);
                if (err != OK)
                        return err;
                err = _free_arg_list(expr->lambda.args);
                break;
        default:
                return xl_raise(ERR_BAD_TYPE, "unknown expr type in free");
        }

        if (err != OK)
                return err;
        free(expr);
        return OK;
}

no_ignore static xl_error
_free_type_expr(struct xl_ast_type_expr *type_expr)
{
        xl_error err;

        switch (type_expr->type_expr_type)
        {
        case TYPE_EXPR_ATOM:
                free(type_expr->name);
                break;

        case TYPE_EXPR_APPLY:
                err = _free_type_expr(type_expr->apply.head);
                if (err != OK)
                        return err;
                err = _free_type_expr(type_expr->apply.tail);
                if (err != OK)
                        return err;
                break;
        }

        free(type_expr);
        return OK;
}

no_ignore static xl_error
_free_binding(struct xl_ast_binding *binding)
{
        xl_error err;

        free(binding->name);

        err = _free_expr(binding->expr);
        if (err != OK)
                return err;

        if (binding->type_expr != NULL)
        {
                err = _free_type_expr(binding->type_expr);
                if (err != OK)
                        return err;
        }

        free(binding);
        return OK;
}

no_ignore xl_error
xl_ast_free(struct xl_ast *ast)
{
        size_t i;
        xl_error err;

        for (i = 0; i < ast->n_bindings; i++)
        {
                err = _free_binding(ast->bindings[i]);
                if (err != OK)
                        return err;
        }
        free(ast->bindings);

        if (ast->immediate != NULL)
        {
                err = _free_expr(ast->immediate);
                if (err != OK)
                        return err;
        }

        free(ast);
        return OK;
}

no_ignore static xl_error
_print_atom(struct xl_ast_atom *atom)
{
        switch (atom->atom_type)
        {
        case ATOM_INT:
                printf("%ld:i", (int64_t) atom->integer);
                return OK;
        case ATOM_NUM:
                printf("%f:f", atom->number);
                return OK;
        case ATOM_NAME:
                printf("%s:n", atom->str);
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
_print_expr(struct xl_ast_expr *expr)
{
        xl_error err;

        switch (expr->expr_type)
        {
        case EXPR_ATOM:
                return _print_atom(expr->atom);

        case EXPR_APPLY:
                printf("(");
                err = _print_expr(expr->apply.head);
                if (err != OK)
                        return err;
                printf(" ");
                err = _print_expr(expr->apply.tail);
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
                err = _print_expr(expr->lambda.body);
                if (err != OK)
                        return err;
                return OK;
        }

        return xl_raise(ERR_UNKNOWN_TYPE, "unknown expr type");
}

no_ignore static xl_error
_print_type(struct xl_ast_type_expr *type_expr)
{
        xl_error err;

        switch (type_expr->type_expr_type)
        {
        case TYPE_EXPR_ATOM:
                printf("%s", type_expr->name);
                return OK;
        case TYPE_EXPR_APPLY:
                err = _print_type(type_expr->apply.head);
                if (err != OK)
                        return err;
                printf(" -> (");
                err = _print_type(type_expr->apply.tail);
                if (err != OK)
                        return err;
                printf(")");
                return OK;
        }
        return xl_raise(ERR_UNKNOWN_TYPE, "unknown type expr type");
}

no_ignore xl_error
xl_ast_print(struct xl_ast *ast)
{
        size_t i;
        xl_error err;
        struct xl_ast_binding *b;

        printf("%lu bindings:\n", ast->n_bindings);
        for (i = 0; i < ast->n_bindings; i++)
        {
                b = ast->bindings[i];
                printf("\tbind %s", b->name);
                if (b->type_expr != NULL)
                {
                        printf(" ^ ");
                        err = _print_type(b->type_expr);
                        if (err != OK)
                                return err;
                }
                printf(" = ");
                err = _print_expr(b->expr);
                if (err != OK)
                        return err;
                printf("\n");
        }

        return OK;
}

no_ignore xl_error
xl_ast_bind(struct xl_ast *ast, struct xl_ast_binding *bind)
{
        struct xl_ast_binding **temp;
        size_t new_cap;

        if (ast->n_bindings < ast->cap_bindings)
        {
                ast->bindings[ast->n_bindings++] = bind;
                return OK;
        }

        new_cap = ast->cap_bindings == 0 ? 8 : 2 * ast->cap_bindings;
        temp = realloc(
                ast->bindings,
                new_cap * sizeof(struct xl_ast_binding *));
        if (temp == NULL)
                return xl_raise(ERR_NO_MEMORY, "ast bindings realloc");
        ast->cap_bindings = new_cap;
        ast->bindings = temp;
        ast->bindings[ast->n_bindings++] = bind;
        return OK;
}

no_ignore xl_error
xl_ast_set_immediate(struct xl_ast *ast, struct xl_ast_expr *expr)
{
        if (ast->immediate != NULL)
                return xl_raise(ERR_PRESENT, "immediate already set");
        ast->immediate = expr;
        return OK;
}

no_ignore xl_error
xl_ast_binding_new(
        struct xl_ast_binding **binding,
        char *name,
        struct xl_ast_expr *expr,
        struct xl_ast_type_expr *type_expr)
{
        check_alloc(*binding, 1, struct xl_ast_binding);

        (*binding)->name = name;
        (*binding)->expr = expr;
        (*binding)->type_expr = type_expr;
        return OK;
}

no_ignore xl_error
xl_ast_expr_new_apply(
        struct xl_ast_expr **expr,
        struct xl_ast_expr *head,
        struct xl_ast_expr *tail)
{
        check_alloc(*expr, 1, struct xl_ast_expr);

        (*expr)->expr_type = EXPR_APPLY;
        (*expr)->apply.head = head;
        (*expr)->apply.tail = tail;

        return OK;
}

no_ignore xl_error
xl_ast_expr_new_lambda(
        struct xl_ast_expr **expr,
        struct xl_ast_arg_list *args,
        struct xl_ast_expr *body)
{
        check_alloc(*expr, 1, struct xl_ast_expr);

        (*expr)->expr_type = EXPR_LAMBDA;
        (*expr)->lambda.args = args;
        (*expr)->lambda.body = body;

        return OK;
}

no_ignore xl_error
xl_ast_expr_new_atom(
        struct xl_ast_expr **expr,
        struct xl_ast_atom *value)
{
        check_alloc(*expr, 1, struct xl_ast_expr);
        (*expr)->expr_type = EXPR_ATOM;
        (*expr)->atom = value;
        return OK;
}

no_ignore xl_error
xl_ast_atom_new_name(
        struct xl_ast_atom **atom,
        char *name)
{
        check_alloc(*atom, 1, struct xl_ast_atom);
        (*atom)->atom_type = ATOM_NAME;
        (*atom)->str = name;
        return OK;
}

no_ignore xl_error
xl_ast_atom_new_type_name(
        struct xl_ast_atom **atom,
        char *type_name)
{
        check_alloc(*atom, 1, struct xl_ast_atom);
        (*atom)->atom_type = ATOM_TYPE_NAME;
        (*atom)->str = type_name;
        return OK;
}

no_ignore xl_error
xl_ast_atom_new_integer(
        struct xl_ast_atom **atom,
        xl_word integer)
{
        check_alloc(*atom, 1, struct xl_ast_atom);
        (*atom)->atom_type = ATOM_INT;
        (*atom)->integer = integer;
        return OK;
}

no_ignore xl_error
xl_ast_atom_new_number(
        struct xl_ast_atom **atom,
        xl_float number)
{
        check_alloc(*atom, 1, struct xl_ast_atom);
        (*atom)->atom_type = ATOM_NUM;
        (*atom)->number = number;
        return OK;
}

no_ignore xl_error
xl_ast_atom_new_string(
        struct xl_ast_atom **atom,
        char *string)
{
        check_alloc(*atom, 1, struct xl_ast_atom);
        (*atom)->atom_type = ATOM_STRING;
        (*atom)->str = string;
        return OK;
}

/* Type expression builders */
no_ignore xl_error
xl_ast_type_expr_new_atom(
        struct xl_ast_type_expr **expr,
        char *type_name)
{
        check_alloc(*expr, 1, struct xl_ast_type_expr);
        (*expr)->name = type_name;
        (*expr)->type_expr_type = TYPE_EXPR_ATOM;
        return OK;
}

no_ignore xl_error
xl_ast_type_expr_new_apply(
        struct xl_ast_type_expr **expr,
        struct xl_ast_type_expr *head,
        struct xl_ast_type_expr *tail)
{
        check_alloc(*expr, 1, struct xl_ast_type_expr);
        (*expr)->type_expr_type = TYPE_EXPR_APPLY;
        (*expr)->apply.head = head;
        (*expr)->apply.tail = tail;
        return OK;
}

no_ignore xl_error
xl_ast_arg_list_new_empty(
        struct xl_ast_arg_list **arg_list)
{
        check_alloc(*arg_list, 1, struct xl_ast_arg_list);
        (*arg_list)->name = NULL;
        (*arg_list)->next = NULL;
        return OK;
}

no_ignore xl_error
xl_ast_arg_list_new_pushl(
        struct xl_ast_arg_list **arg_list,
        char *head,
        struct xl_ast_arg_list *tail)
{
        check_alloc(*arg_list, 1, struct xl_ast_arg_list);
        (*arg_list)->name = head;
        (*arg_list)->next = tail;
        return OK;
}

no_ignore xl_error
xl_ast_import(
        struct xl_ast *ast,
        struct xl_ast_import_list *import_list)
{
        import_list->next = ast->imports;
        ast->imports = import_list;
        return OK;
}

no_ignore xl_error
xl_ast_import_list_new(
        struct xl_ast_import_list **import_list,
        char *head)
{
        check_alloc(*import_list, 1, struct xl_ast_import_list);
        (*import_list)->name = head;
        return OK;
}
