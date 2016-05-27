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

#include <stdlib.h>
#include <string.h>

#define check_alloc(x, nelem, contents) { \
        (x) = calloc(nelem, sizeof(contents)); \
        if ((x) == NULL) return ubik_raise(ERR_NO_MEMORY, ""); }

no_ignore static ubik_error
_free_expr(struct ubik_ast_expr *expr);

/* Allocates a new AST. */
no_ignore ubik_error
ubik_ast_new(struct ubik_ast **ast)
{
        check_alloc(*ast, 1, struct ubik_ast);
        return OK;
}

no_ignore static ubik_error
_free_atom(struct ubik_ast_atom *atom)
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
                return ubik_raise(ERR_BAD_TYPE, "unknown atom type in free");
        }

        free(atom);
        return OK;
}

no_ignore static ubik_error
_free_case_stmts(struct ubik_ast_case *case_stmt)
{
        struct ubik_ast_case *next;
        ubik_error err;

        while (case_stmt != NULL)
        {
                next = case_stmt->next;
                if (case_stmt->head != NULL)
                {
                        err = _free_expr(case_stmt->head);
                        if (err != OK)
                                return err;
                }
                err = _free_expr(case_stmt->tail);
                if (err != OK)
                        return err;
                if (case_stmt->gen != NULL)
                        free(case_stmt->gen);
                free(case_stmt);
                case_stmt = next;
        }

        return OK;
}

no_ignore static ubik_error
_free_arg_list(struct ubik_ast_arg_list *arg_list)
{
        struct ubik_ast_arg_list *next;

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

no_ignore static ubik_error
_free_expr(struct ubik_ast_expr *expr)
{
        ubik_error err;

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
                err = ubik_ast_free(expr->constructor.scope);
                break;
        case EXPR_BLOCK:
                err = ubik_ast_free(expr->block);
                break;
        case EXPR_COND_BLOCK:
                err = _free_case_stmts(expr->cond_block.case_stmts);
                if (err != OK)
                        return err;
                switch (expr->cond_block.block_type)
                {
                case COND_PATTERN:
                        err = _free_expr(expr->cond_block.to_match);
                        break;
                case COND_PREDICATE:
                        break;
                }
                break;
        default:
                return ubik_raise(ERR_BAD_TYPE, "unknown expr type in free");
        }

        if (err != OK)
                return err;

        if (expr->gen != NULL)
                free(expr->gen);
        free(expr);
        return OK;
}

no_ignore static ubik_error
_free_type_expr(struct ubik_ast_type_expr *type_expr)
{
        ubik_error err;

        switch (type_expr->type_expr_type)
        {
        case TYPE_EXPR_ATOM:
        case TYPE_EXPR_VAR:
                free(type_expr->name);
                break;

        case TYPE_EXPR_APPLY:
        case TYPE_EXPR_ARROW:
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

no_ignore static ubik_error
_free_binding(struct ubik_ast_binding *binding)
{
        ubik_error err;

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

no_ignore static ubik_error
_free_member_list(struct ubik_ast_member_list *member_list)
{
        ubik_error err;

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

no_ignore static ubik_error
_free_type_params(struct ubik_ast_type_params *p)
{
        struct ubik_ast_type_params *to_free;

        while (p != NULL)
        {
                free(p->name);
                to_free = p;
                p = p->next;
                free(to_free);
        }
        return OK;
}

no_ignore static ubik_error
_free_type_constraints(struct ubik_ast_type_constraints *c)
{
        struct ubik_ast_type_constraints *to_free;
        ubik_error err;

        while (c != NULL)
        {
                free(c->interface);
                err = _free_type_params(c->params);
                if (err != OK)
                        return err;
                to_free = c;
                c = c->next;
                free(to_free);
        }

        return OK;
}

no_ignore static ubik_error
_free_type_list(struct ubik_ast_type_list *l)
{
        struct ubik_ast_type_list *to_free;
        ubik_error err;

        while (l != NULL)
        {
                err = _free_type_expr(l->type_expr);
                if (err != OK)
                        return err;
                to_free = l;
                l = l->next;
                free(to_free);
        }

        return OK;
}

no_ignore static ubik_error
_free_adt_ctors(struct ubik_ast_adt_ctors *c)
{
        struct ubik_ast_adt_ctors *to_free;
        ubik_error err;

        while (c != NULL)
        {
                free(c->name);
                err = _free_type_list(c->params);
                if (err != OK)
                        return err;
                to_free = c;
                c = c->next;
                free(to_free);
        }

        return OK;
}

no_ignore static ubik_error
_free_type(struct ubik_ast_type *type)
{
        ubik_error err;

        free(type->name);

        switch (type->type)
        {
        case TYPE_RECORD:
                err = _free_member_list(type->members);
                if (err != OK)
                        return err;
                break;

        case TYPE_ADT:
                err = _free_type_params(type->adt.params);
                if (err != OK)
                        return err;
                err = _free_type_constraints(type->adt.constraints);
                if (err != OK)
                        return err;
                err = _free_adt_ctors(type->adt.ctors);
                if (err != OK)
                        return err;
                break;

        case TYPE_ALIAS:
                err = _free_type_expr(type->aliases_to);
                if (err != OK)
                        return err;
                break;

        default:
                return ubik_raise(ERR_BAD_TYPE, "unknown type type in free");
        }

        free(type);
        return OK;
}

no_ignore static ubik_error
_free_import_list(struct ubik_ast_import_list *import_list)
{
        struct ubik_ast_import_list *to_free;

        while (import_list != NULL)
        {
                to_free = import_list;
                import_list = to_free->next;

                free(to_free->name);
                free(to_free);
        }

        return OK;
}

no_ignore ubik_error
ubik_ast_free(struct ubik_ast *ast)
{
        size_t i;
        ubik_error err;

        for (i = 0; i < ast->bindings.n; i++)
        {
                err = _free_binding(ast->bindings.elems[i]);
                if (err != OK)
                        return err;
        }
        ubik_vector_free(&ast->bindings);

        for (i = 0; i < ast->types.n; i++)
        {
                err = _free_type(ast->types.elems[i]);
                if (err != OK)
                        return err;
        }
        ubik_vector_free(&ast->types);

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

no_ignore ubik_error
ubik_ast_bind(struct ubik_ast *ast, struct ubik_ast_binding *bind)
{
        return ubik_vector_append(&ast->bindings, bind);
}

no_ignore ubik_error
ubik_ast_add_type(struct ubik_ast *ast, struct ubik_ast_type *type)
{
        return ubik_vector_append(&ast->types, type);
}

no_ignore ubik_error
ubik_ast_atom_new_qualified(
        struct ubik_ast_atom **atom,
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

        ubik_assert(head_len > 0);
        ubik_assert(tail_len > 0);

        check_alloc(*atom, 1, struct ubik_ast_atom);
        (*atom)->atom_type = ATOM_QUALIFIED;

        (*atom)->qualified.head = calloc(head_len + 1, sizeof(char));
        if ((*atom)->qualified.head == NULL)
                return ubik_raise(ERR_NO_MEMORY, "qualified alloc");
        memcpy((*atom)->qualified.head, name, head_len);

        (*atom)->qualified.tail = calloc(tail_len + 1, sizeof(char));
        if ((*atom)->qualified.tail == NULL)
                return ubik_raise(ERR_NO_MEMORY, "qualified alloc");
        memcpy((*atom)->qualified.tail, &name[head_len + 1], tail_len);

        free(name);

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

        *subast = NULL;
        *n_subexprs = 0;

        switch (expr->expr_type)
        {
        case EXPR_ATOM:
                return OK;

        case EXPR_APPLY:
                subexprs[0] = expr->apply.head;
                subexprs[1] = expr->apply.tail;
                *n_subexprs = 2;
                return OK;

        case EXPR_LAMBDA:
                subexprs[0] = expr->lambda.body;
                *n_subexprs = 1;
                return OK;

        case EXPR_CONSTRUCTOR:
                *subast = expr->constructor.scope;
                return OK;

        case EXPR_COND_BLOCK:
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
                        if (case_stmt->head != NULL)
                                subexprs[i++] = case_stmt->head;
                        subexprs[i++] = case_stmt->tail;
                        case_stmt = case_stmt->next;
                }
                *n_subexprs = i;
                return OK;

        case EXPR_BLOCK:
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
}
