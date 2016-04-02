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

#include "expel/assert.h"
#include "expel/ast.h"

#include <stdlib.h>
#include <string.h>

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

        case ATOM_QUALIFIED:
                free(atom->qualified.head);
                free(atom->qualified.tail);

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
                if (arg_list->gen != NULL)
                        free(arg_list->gen);
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
        case EXPR_CONSTRUCTOR:
                free(expr->constructor.type_name);
                err = xl_ast_free(expr->constructor.scope);
                break;
        case EXPR_CONDITIONAL:
                err = _free_expr(expr->condition.cond);
                if (err != OK)
                        return err;
                err = _free_expr(expr->condition.implied);
                if (err != OK)
                        return err;
                err = _free_expr(expr->condition.opposed);
                break;
        case EXPR_BLOCK:
                err = xl_ast_free(expr->block);
                break;
        default:
                return xl_raise(ERR_BAD_TYPE, "unknown expr type in free");
        }

        if (err != OK)
                return err;

        if (expr->gen != NULL)
                free(expr->gen);
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

no_ignore static xl_error
_free_member_list(struct xl_ast_member_list *member_list)
{
        xl_error err;

        if (member_list->next)
        {
                err = _free_member_list(member_list->next);
                if (err != OK)
                        return err;
        }

        free(member_list->name);

        err = _free_type_expr(member_list->type);
        if (err != OK)
                return err;

        free(member_list);
        return OK;
}

no_ignore static xl_error
_free_type(struct xl_ast_type *type)
{
        xl_error err;

        free(type->name);

        switch (type->type)
        {
        case TYPE_RECORD:
                err = _free_member_list(type->members);
                if (err != OK)
                        return err;
                break;

        default:
                return xl_raise(ERR_BAD_TYPE, "unknown type type in free");
        }

        free(type);
        return OK;
}

no_ignore static xl_error
_free_import_list(struct xl_ast_import_list *import_list)
{
        struct xl_ast_import_list *to_free;

        while (import_list != NULL)
        {
                to_free = import_list;
                import_list = to_free->next;

                free(to_free->name);
                free(to_free);
        }

        return OK;
}

no_ignore xl_error
xl_ast_free(struct xl_ast *ast)
{
        size_t i;
        xl_error err;

        for (i = 0; i < ast->bindings.n; i++)
        {
                err = _free_binding(ast->bindings.elems[i]);
                if (err != OK)
                        return err;
        }
        xl_vector_free(&ast->bindings);

        for (i = 0; i < ast->types.n; i++)
        {
                err = _free_type(ast->types.elems[i]);
                if (err != OK)
                        return err;
        }
        xl_vector_free(&ast->types);

        if (ast->immediate != NULL)
        {
                err = _free_expr(ast->immediate);
                if (err != OK)
                        return err;
        }

        if (ast->imports != NULL)
        {
                err = _free_import_list(ast->imports);
                if (err != OK)
                        return err;
        }

        free(ast);
        return OK;
}

no_ignore xl_error
xl_ast_bind(struct xl_ast *ast, struct xl_ast_binding *bind)
{
        return xl_vector_append(&ast->bindings, bind);
}

no_ignore xl_error
xl_ast_add_type(struct xl_ast *ast, struct xl_ast_type *type)
{
        return xl_vector_append(&ast->types, type);
}

no_ignore xl_error
xl_ast_atom_new_qualified(
        struct xl_ast_atom **atom,
        char *name)
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

        xl_assert(head_len > 0);
        xl_assert(tail_len > 0);

        check_alloc(*atom, 1, struct xl_ast_atom);
        (*atom)->atom_type = ATOM_QUALIFIED;

        (*atom)->qualified.head = calloc(head_len + 1, sizeof(char));
        if ((*atom)->qualified.head == NULL)
                return xl_raise(ERR_NO_MEMORY, "qualified alloc");
        memcpy((*atom)->qualified.head, name, head_len);

        (*atom)->qualified.tail = calloc(tail_len + 1, sizeof(char));
        if ((*atom)->qualified.tail == NULL)
                return xl_raise(ERR_NO_MEMORY, "qualified alloc");
        memcpy((*atom)->qualified.tail, &name[head_len + 1], tail_len);

        free(name);

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
xl_ast_subexprs(
        struct xl_ast **subast,
        struct xl_ast_expr **subexprs,
        size_t *n_subexprs,
        struct xl_ast_expr *expr)
{

}
