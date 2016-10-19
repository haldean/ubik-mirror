/*
 * assign.c: node assignment
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

#include <stdlib.h>
#include <string.h>

#include "ubik/assert.h"
#include "ubik/assign.h"
#include "ubik/feedback.h"
#include "ubik/fun.h"
#include "ubik/resolve.h"
#include "ubik/rt.h"
#include "ubik/rttypes.h"
#include "ubik/streamutil.h"
#include "ubik/uri.h"
#include "ubik/value.h"
#include "ubik/vector.h"

no_ignore static ubik_error
_add_nontotal_predicate_err(
        struct ubik_assign_context *ctx,
        struct ubik_ast_loc loc)
{
        struct ubik_assign_error *res;
        ubik_error err;

        ubik_alloc1(&res, struct ubik_assign_error, ctx->region);
        res->err_type = ASSIGN_ERR_PRED_BLOCK_NOT_TOTAL;
        res->loc = loc;

        err = ubik_vector_append(&ctx->errors, res);
        if (err != OK)
                return err;
        return OK;
}

no_ignore static ubik_error
_set_name_in_scope(
        struct ubik_resolve_scope *scope,
        char *name,
        ubik_word node)
{
        struct ubik_resolve_name *bind;
        size_t i;
        bool found;

        bind = NULL;
        found = false;

        for (i = 0; i < scope->names.n && !found; i++)
        {
                bind = scope->names.elems[i];
                if (strcmp(name, bind->name) == 0)
                        found = true;
        }

        if (!found)
                return ubik_raise(ERR_ABSENT, "name not found in scope");

        bind->node = node;
        return OK;
}

no_ignore static ubik_error
_find_name_in_scope(
        ubik_word *n,
        struct ubik_resolve_scope *scope,
        char *name)
{
        struct ubik_resolve_name *bind;
        size_t i;
        bool found;

        found = false;

        do
        {
                for (i = 0; i < scope->names.n && !found; i++)
                {
                        bind = scope->names.elems[i];
                        if (strcmp(name, bind->name) == 0)
                                found = true;
                }
                scope = scope->parent;
        }
        while (!found && scope != NULL);

        if (!found)
                return ubik_raise(ERR_ABSENT, "name not found in scope");

        *n = bind->node;
        return OK;
}

no_ignore static ubik_error
_assign_atom_node(
        struct ubik_assign_context *ctx,
        struct ubik_node *n,
        struct ubik_ast_expr *expr)
{
        ubik_word referrent;
        enum ubik_resolve_type res_type;
        char *pkg;
        ubik_error err;

        switch (expr->atom->atom_type)
        {
        case ATOM_INT:
                n->node_type = UBIK_VALUE;

                err = ubik_value_new(&n->value.type, ctx->workspace);
                if (err != OK)
                        return err;
                err = ubik_type_rat(n->value.type);
                if (err != OK)
                        return err;

                err = ubik_value_new(&n->value.value, ctx->workspace);
                if (err != OK)
                        return err;
                n->value.value->type = UBIK_RAT;
                n->value.value->rat.num = expr->atom->integer;
                n->value.value->rat.den = 1;
                return OK;

        case ATOM_NUM:
                return ubik_raise(ERR_NOT_IMPLEMENTED, "rationals?!");

        case ATOM_VALUE:
                n->node_type = UBIK_VALUE;

                err = ubik_value_new(&n->value.type, ctx->workspace);
                if (err != OK)
                        return err;
                /* TODO: type these! */
                err = ubik_type_rat(n->value.type);
                if (err != OK)
                        return err;

                n->value.value = expr->atom->value;
                return OK;

        case ATOM_TYPE_NAME:
        case ATOM_NAME:
                res_type = expr->atom->name_loc->type;

                if (res_type == RESOLVE_GLOBAL || res_type == RESOLVE_NATIVE)
                {
                        n->node_type = UBIK_LOAD;

                        ubik_galloc1(&n->load.loc, struct ubik_uri);
                        if (res_type == RESOLVE_NATIVE)
                                err = ubik_uri_native(
                                        n->load.loc, expr->atom->str);
                        else
                        {
                                pkg = expr->atom->name_loc->package_name;
                                ubik_assert(pkg != NULL);
                                err = ubik_uri_package(
                                        n->load.loc, pkg, expr->atom->str);
                        }
                        if (err != OK)
                        {
                                free(n->load.loc);
                                return err;
                        }
                        return OK;
                }

                referrent = UBIK_INVALID_NODE_ID;
                err = _find_name_in_scope(
                        &referrent, expr->scope, expr->atom->str);
                if (err != OK)
                        return err;
                if (referrent == UBIK_INVALID_NODE_ID)
                        return ubik_raise(ERR_ABSENT, "no node set on name");

                n->node_type = UBIK_REF;
                n->ref.referrent = referrent;
                return OK;

        case ATOM_QUALIFIED:
                n->node_type = UBIK_LOAD;

                ubik_galloc1(&n->load.loc, struct ubik_uri);

                err = ubik_uri_package(
                        n->load.loc,
                        expr->atom->qualified.head,
                        expr->atom->qualified.tail);
                if (err != OK)
                {
                        free(n->load.loc);
                        return err;
                }

                return OK;

        case ATOM_STRING:
                n->node_type = UBIK_VALUE;

                err = ubik_value_new(&n->value.type, ctx->workspace);
                if (err != OK)
                        return err;
                err = ubik_type_str(n->value.type);
                if (err != OK)
                        return err;

                err = ubik_value_new(&n->value.value, ctx->workspace);
                if (err != OK)
                        return err;
                n->value.value->type = UBIK_STR;
                n->value.value->str.length = strlen(expr->atom->str);
                n->value.value->str.data = strdup(expr->atom->str);
                return OK;
        }
        return ubik_raise(ERR_UNKNOWN_TYPE, "compile atom type");
}

no_ignore static ubik_error
_assign_apply_node(
        struct ubik_node *n,
        struct ubik_ast_expr *expr)
{
        n->node_type = UBIK_APPLY;
        n->apply.func = expr->apply.head->gen;
        n->apply.arg = expr->apply.tail->gen;
        return OK;
}

no_ignore static ubik_error
_assign_pred_case(
        struct ubik_assign_context *ctx,
        struct ubik_vector *nodes,
        struct ubik_ast_case *case_stmt)
{
        struct ubik_node *n;
        ubik_error err;

        ubik_alloc1(&n, struct ubik_node, ctx->region);

        if (case_stmt->next != NULL)
        {
                err = _assign_pred_case(ctx, nodes, case_stmt->next);
                if (err != OK)
                        goto failed;

                err = ubik_assign_nodes(ctx, nodes, case_stmt->head);
                if (err != OK)
                        goto failed;

                err = ubik_assign_nodes(ctx, nodes, case_stmt->tail);
                if (err != OK)
                        goto failed;

                n->node_type = UBIK_COND;
                n->cond.condition = case_stmt->head->gen;
                n->cond.if_true = case_stmt->tail->gen;
                n->cond.if_false = case_stmt->next->gen;
        }
        else
        {
                /* This is a user error but we still have enough information to
                 * go ahead and generate code. We rely on the caller to pay
                 * attention to the errors present in the context after
                 * assignment is complete. */
                if (case_stmt->head != NULL)
                {
                        err = _add_nontotal_predicate_err(ctx, case_stmt->loc);
                        if (err != OK)
                                goto failed;
                }
                err = ubik_assign_nodes(ctx, nodes, case_stmt->tail);
                if (err != OK)
                        goto failed;

                n->node_type = UBIK_REF;
                n->ref.referrent = case_stmt->tail->gen;
        }

        n->id = nodes->n;
        err = ubik_vector_append(nodes, n);
        if (err != OK)
                goto failed;
        case_stmt->gen = n->id;

        return OK;

failed:
        free(n);
        return err;
}

no_ignore static ubik_error
_assign_pred_block(
        struct ubik_assign_context *ctx,
        struct ubik_node *n,
        struct ubik_vector *nodes,
        struct ubik_ast_expr *expr)
{
        ubik_error err;

        err = _assign_pred_case(ctx, nodes, expr->cond_block.case_stmts);
        if (err != OK)
                return err;

        n->node_type = UBIK_REF;
        n->id = nodes->n;
        n->ref.referrent = expr->cond_block.case_stmts->gen;

        return OK;
}

no_ignore static ubik_error
_assign_block(
        struct ubik_assign_context *ctx,
        struct ubik_node *n,
        struct ubik_vector *nodes,
        struct ubik_ast_expr *expr)
{
        size_t i;
        struct ubik_ast_binding *bind;
        struct ubik_node *subnode;
        ubik_error err;
        ubik_word node_index;

        if (expr->block->types.n != 0)
                return ubik_raise(ERR_NOT_IMPLEMENTED, "scoped types");

        /* first set all of the binding nodes to REF nodes, so that things
         * can point at the definitions of names before the names actually
         * exist. This also allows for circular references between binding
         * expressions. */
        for (i = 0; i < expr->block->bindings.n; i++)
        {
                bind = expr->block->bindings.elems[i];

                ubik_alloc1(&subnode, struct ubik_node, ctx->region);
                subnode->node_type = UBIK_REF;
                subnode->id = nodes->n;
                /* leave referrent set to an invalid value; we'll set it after
                   we generate a node for this name. */
                subnode->ref.referrent = UBIK_INVALID_NODE_ID;

                err = ubik_vector_append(nodes, subnode);
                if (err != OK)
                        return err;

                err = _set_name_in_scope(
                        expr->block->scope,
                        bind->name,
                        subnode->id);
                if (err != OK)
                        return err;
        }

        for (i = 0; i < expr->block->bindings.n; i++)
        {
                bind = expr->block->bindings.elems[i];
                err = ubik_assign_nodes(ctx, nodes, bind->expr);
                if (err != OK)
                        return err;

                err = _find_name_in_scope(
                        &node_index, expr->block->scope, bind->name);
                if (err != OK)
                        return err;
                subnode = (struct ubik_node *) nodes->elems[i];
                subnode->ref.referrent = bind->expr->gen;
        }

        err = ubik_assign_nodes(ctx, nodes, expr->block->immediate);
        if (err != OK)
                return err;
        n->node_type = UBIK_REF;
        n->ref.referrent = expr->block->immediate->gen;
        return OK;
}

no_ignore static ubik_error
_assign_lambda(
        struct ubik_assign_context *ctx,
        struct ubik_node *n,
        struct ubik_ast_expr *expr)
{
        struct ubik_value *subgraph;
        struct ubik_ast_arg_list *t;
        struct ubik_node *input_node;
        struct ubik_vector subnodes = {.region = ctx->region};
        size_t i;
        ubik_error err;

        t = expr->lambda.args;
        i = 0;
        for (i = 0, t = expr->lambda.args;
             t != NULL && t->name != NULL;
             i++, t = t->next)
        {
                ubik_alloc1(&input_node, struct ubik_node, ctx->region);
                input_node->node_type = UBIK_INPUT;
                input_node->id = subnodes.n;
                input_node->input.arg_num = i;

                err = ubik_vector_append(&subnodes, input_node);
                if (err != OK)
                        return err;
                t->gen = input_node->id;

                err = _set_name_in_scope(expr->scope, t->name, t->gen);
                if (err != OK)
                        return err;
        }

        err = ubik_assign_nodes(ctx, &subnodes, expr->lambda.body);
        if (err != OK)
                return err;

        err = ubik_value_new(&subgraph, ctx->workspace);
        if (err != OK)
                return err;
        ubik_fun_from_vector(subgraph, &subnodes, expr->lambda.body->gen);

        n->node_type = UBIK_VALUE;
        n->value.value = subgraph;

        /* TODO: lambda type here. */
        err = ubik_value_new(&n->value.type, ctx->workspace);
        if (err != OK)
                return err;

        return OK;
}

no_ignore ubik_error
ubik_assign_nodes(
        struct ubik_assign_context *ctx,
        struct ubik_vector *nodes,
        struct ubik_ast_expr *expr)
{
        struct ubik_node *n;
        ubik_error err;

        ubik_alloc1(&n, struct ubik_node, ctx->region);

        switch (expr->expr_type)
        {
        case EXPR_ATOM:
                err = _assign_atom_node(ctx, n, expr);
                if (err != OK)
                        return err;
                break;

        case EXPR_APPLY:
                err = ubik_assign_nodes(ctx, nodes, expr->apply.head);
                if (err != OK)
                        return err;

                err = ubik_assign_nodes(ctx, nodes, expr->apply.tail);
                if (err != OK)
                        return err;

                err = _assign_apply_node(n, expr);
                if (err != OK)
                        return err;
                break;

        case EXPR_LAMBDA:
                err = _assign_lambda(ctx, n, expr);
                if (err != OK)
                        return err;
                break;

        case EXPR_BLOCK:
                err = _assign_block(ctx, n, nodes, expr);
                if (err != OK)
                        return err;
                break;

        case EXPR_COND_BLOCK:
                switch (expr->cond_block.block_type)
                {
                case COND_PREDICATE:
                        err = _assign_pred_block(ctx, n, nodes, expr);
                        if (err != OK)
                                return err;
                        break;
                case COND_PATTERN:
                        err = ubik_raise(
                                ERR_UNEXPECTED_FAILURE,
                                "pattern blocks should have been transformed");
                        return err;
                }
                break;

        default:
                err = ubik_raise(ERR_UNKNOWN_TYPE, "compile assign node");
                return err;
        }

        n->id = nodes->n;
        err = ubik_vector_append(nodes, n);
        if (err != OK)
                return err;
        expr->gen = n->id;

        return OK;
}

bool
ubik_assign_emit_errors(struct ubik_assign_context *ctx)
{
        size_t i;
        struct ubik_assign_error *ass_err;

        for (i = 0; i < ctx->errors.n; i++)
        {
                ass_err = ctx->errors.elems[i];
                switch (ass_err->err_type)
                {
                case ASSIGN_ERR_PRED_BLOCK_NOT_TOTAL:
                        ubik_feedback_error_line(
                                ctx->feedback,
                                UBIK_FEEDBACK_ERR,
                                &ass_err->loc,
                                "last case in predicate block must have an "
                                "empty head");
                        break;
                }
        }

        return ctx->errors.n != 0;
}

void
ubik_assign_context_free(struct ubik_assign_context *ctx)
{
        unused(ctx);
}
