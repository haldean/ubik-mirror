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
#include "ubik/dagc.h"
#include "ubik/feedback.h"
#include "ubik/resolve.h"
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
        struct ubik_dagc_node *n)
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

        bind->node = n;
        return OK;
}

no_ignore static ubik_error
_find_name_in_scope(
        struct ubik_dagc_node **n,
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
        union ubik_dagc_any_node *n,
        struct ubik_ast_expr *expr)
{
        struct ubik_dagc_node *referrent;
        enum ubik_resolve_type res_type;
        char *pkg;
        ubik_error err;

        switch (expr->atom->atom_type)
        {
        case ATOM_INT:
                n->node.node_type = DAGC_NODE_CONST;
                n->node.id = ctx->next_id++;

                err = ubik_value_new(&n->as_const.type);
                if (err != OK)
                        return err;
                err = ubik_type_word(n->as_const.type);
                if (err != OK)
                        return err;

                err = ubik_value_new(&n->as_const.value.tree);
                if (err != OK)
                        return err;
                n->as_const.value.tree->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
                n->as_const.value.tree->left.w = expr->atom->integer;
                return OK;

        case ATOM_NUM:
                n->node.node_type = DAGC_NODE_CONST;
                n->node.id = ctx->next_id++;

                err = ubik_value_new(&n->as_const.type);
                if (err != OK)
                        return err;
                err = ubik_type_float(n->as_const.type);
                if (err != OK)
                        return err;

                err = ubik_value_new(&n->as_const.value.tree);
                if (err != OK)
                        return err;
                n->as_const.value.tree->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
                n->as_const.value.tree->left.f = expr->atom->number;
                return OK;

        case ATOM_VALUE:
                n->node.node_type = DAGC_NODE_CONST;
                n->node.id = ctx->next_id++;

                err = ubik_value_new(&n->as_const.type);
                if (err != OK)
                        return err;
                /* TODO: type these! */
                err = ubik_type_word(n->as_const.type);
                if (err != OK)
                        return err;

                err = ubik_take(expr->atom->value);
                if (err != OK)
                        return err;
                n->as_const.value.tree = expr->atom->value;
                return OK;

        case ATOM_TYPE_NAME:
        case ATOM_NAME:
                res_type = expr->atom->name_loc->type;

                if (res_type == RESOLVE_GLOBAL || res_type == RESOLVE_NATIVE)
                {
                        n->node.node_type = DAGC_NODE_LOAD;
                        n->node.id = ctx->next_id++;

                        /* globally allocated and refcounted */
                        ubik_uri_alloc(&n->as_load.loc);
                        err = ubik_take(n->as_load.loc);
                        if (err != OK)
                        {
                                free(n->as_load.loc);
                                return err;
                        }

                        if (res_type == RESOLVE_NATIVE)
                                err = ubik_uri_native(
                                        n->as_load.loc, expr->atom->str);
                        else
                        {
                                pkg = expr->atom->name_loc->package_name;
                                ubik_assert(pkg != NULL);
                                err = ubik_uri_package(
                                        n->as_load.loc, pkg, expr->atom->str);
                        }
                        if (err != OK)
                                return err;
                        return OK;
                }

                referrent = NULL;
                err = _find_name_in_scope(
                        &referrent, expr->scope, expr->atom->str);
                if (err != OK)
                        return err;
                if (referrent == NULL)
                        return ubik_raise(ERR_ABSENT, "no node set on name");

                n->node.node_type = DAGC_NODE_REF;
                n->node.id = ctx->next_id++;
                n->as_ref.referrent = referrent;
                return OK;

        case ATOM_QUALIFIED:
                n->node.node_type = DAGC_NODE_LOAD;
                n->node.id = ctx->next_id++;

                /* globally allocated and refcounted */
                ubik_uri_alloc(&n->as_load.loc);
                err = ubik_take(n->as_load.loc);
                if (err != OK)
                {
                        free(n->as_load.loc);
                        return err;
                }

                err = ubik_uri_package(
                        n->as_load.loc,
                        expr->atom->qualified.head,
                        expr->atom->qualified.tail);
                if (err != OK)
                        return err;

                return OK;

        case ATOM_STRING:
                n->node.node_type = DAGC_NODE_CONST;
                n->node.id = ctx->next_id++;

                err = ubik_value_new(&n->as_const.type);
                if (err != OK)
                        return err;
                err = ubik_type_string(n->as_const.type);
                if (err != OK)
                        return err;

                err = ubik_value_new(&n->as_const.value.tree);
                if (err != OK)
                        return err;
                err = ubik_value_pack_string(
                        n->as_const.value.tree, expr->atom->str,
                        strlen(expr->atom->str));
                return OK;
        }
        return ubik_raise(ERR_UNKNOWN_TYPE, "compile atom type");
}

no_ignore static ubik_error
_assign_apply_node(
        struct ubik_assign_context *ctx,
        union ubik_dagc_any_node *n,
        struct ubik_ast_expr *expr)
{
        n->node.node_type = DAGC_NODE_APPLY;
        n->node.id = ctx->next_id++;

        n->as_apply.func = expr->apply.head->gen;
        n->as_apply.arg = expr->apply.tail->gen;

        return OK;
}

no_ignore static ubik_error
_assign_pred_case(
        struct ubik_assign_context *ctx,
        struct ubik_graph_builder *builder,
        struct ubik_ast_case *case_stmt)
{
        union ubik_dagc_any_node *n;
        ubik_error err;

        ubik_alloc1(&n, union ubik_dagc_any_node, ctx->region);

        if (case_stmt->next != NULL)
        {
                err = _assign_pred_case(ctx, builder, case_stmt->next);
                if (err != OK)
                        goto failed;

                err = ubik_assign_nodes(ctx, builder, case_stmt->head);
                if (err != OK)
                        goto failed;

                err = ubik_assign_nodes(ctx, builder, case_stmt->tail);
                if (err != OK)
                        goto failed;

                n->node.node_type = DAGC_NODE_COND;
                n->node.id = ctx->next_id++;

                n->as_cond.condition = case_stmt->head->gen;
                n->as_cond.if_true = case_stmt->tail->gen;
                n->as_cond.if_false = case_stmt->next->gen;
        }
        else
        {
                /* This is a user error but we still have enough information to
                 * go ahead and generate code. We rely on the caller to pay
                 * attention to the errors present in the context after
                 * assignment is complete. */
                if (case_stmt->head != NULL)
                {
                        err = _add_nontotal_predicate_err(
                                ctx, case_stmt->loc);
                        if (err != OK)
                                goto failed;
                }
                err = ubik_assign_nodes(ctx, builder, case_stmt->tail);
                if (err != OK)
                        goto failed;

                n->node.node_type = DAGC_NODE_REF;
                n->node.id = ctx->next_id++;
                n->as_ref.referrent = case_stmt->tail->gen;
        }

        err = ubik_bdagc_push_node(builder, &n->node);
        if (err != OK)
                goto failed;
        case_stmt->gen = &n->node;
        return OK;

failed:
        free(n);
        return err;
}

no_ignore static ubik_error
_assign_pred_block(
        struct ubik_assign_context *ctx,
        union ubik_dagc_any_node *n,
        struct ubik_graph_builder *builder,
        struct ubik_ast_expr *expr)
{
        ubik_error err;

        err = _assign_pred_case(ctx, builder, expr->cond_block.case_stmts);
        if (err != OK)
                return err;

        n->node.node_type = DAGC_NODE_REF;
        n->node.id = ctx->next_id++;
        n->as_ref.referrent = expr->cond_block.case_stmts->gen;

        return OK;
}

no_ignore static ubik_error
_assign_block(
        struct ubik_assign_context *ctx,
        union ubik_dagc_any_node *n,
        struct ubik_graph_builder *builder,
        struct ubik_ast_expr *expr)
{
        size_t i;
        struct ubik_ast_binding *bind;
        union ubik_dagc_any_node *subnode;
        ubik_error err;

        if (expr->block->types.n != 0)
                return ubik_raise(ERR_NOT_IMPLEMENTED, "private types");

        /* first set all of the binding nodes to REF nodes, so that things
         * can point at the definitions of names before the names actually
         * exist. This also allows for circular references between binding
         * expressions. */
        for (i = 0; i < expr->block->bindings.n; i++)
        {
                bind = expr->block->bindings.elems[i];

                ubik_alloc1(&subnode, union ubik_dagc_any_node, ctx->region);
                subnode->node.node_type = DAGC_NODE_REF;
                subnode->node.id = ctx->next_id++;
                /* leave referrent unset; we'll set it after we generate a
                 * node for this name. */

                err = ubik_bdagc_push_node(builder, &subnode->node);
                if (err != OK)
                        return err;

                err = _set_name_in_scope(
                        expr->block->scope,
                        bind->name,
                        &subnode->node);
                if (err != OK)
                        return err;
        }

        for (i = 0; i < expr->block->bindings.n; i++)
        {
                bind = expr->block->bindings.elems[i];
                err = ubik_assign_nodes(ctx, builder, bind->expr);
                if (err != OK)
                        return err;

                err = _find_name_in_scope(
                        (struct ubik_dagc_node **) &subnode,
                        expr->block->scope, bind->name);
                if (err != OK)
                        return err;
                subnode->as_ref.referrent = bind->expr->gen;
        }

        err = ubik_assign_nodes(ctx, builder, expr->block->immediate);
        if (err != OK)
                return err;

        n->node.node_type = DAGC_NODE_REF;
        n->node.id = ctx->next_id++;
        n->as_ref.referrent = expr->block->immediate->gen;
        return OK;
}

no_ignore static ubik_error
_assign_lambda(
        struct ubik_assign_context *ctx,
        union ubik_dagc_any_node *n,
        struct ubik_ast_expr *expr)
{
        struct ubik_graph_builder builder;
        struct ubik_dagc *subgraph;
        struct ubik_ast_arg_list *t;
        struct ubik_dagc_input *input_node;
        size_t i;
        ubik_error err;

        err = ubik_bdagc_init(&builder, ctx->region);
        if (err != OK)
                return err;

        t = expr->lambda.args;
        i = 0;
        while (t != NULL && t->name != NULL)
        {
                ubik_alloc1(&input_node, struct ubik_dagc_input, ctx->region);
                input_node->head.node_type = DAGC_NODE_INPUT;
                input_node->head.id = ctx->next_id++;
                input_node->arg_num = i++;

                err = ubik_bdagc_push_node(
                        &builder, (struct ubik_dagc_node *) input_node);
                if (err != OK)
                {
                        ubik_bdagc_free(&builder);
                        return err;
                }
                t->gen = (struct ubik_dagc_node *) input_node;

                err = _set_name_in_scope(expr->scope, t->name, t->gen);
                if (err != OK)
                {
                        ubik_bdagc_free(&builder);
                        return err;
                }

                t = t->next;
        }

        err = ubik_assign_nodes(ctx, &builder, expr->lambda.body);
        if (err != OK)
        {
                ubik_bdagc_free(&builder);
                return err;
        }

        builder.result = expr->lambda.body->gen;
        builder.result->is_terminal = true;

        err = ubik_bdagc_build(&subgraph, &builder);
        if (err != OK)
                return err;

        /* we let the node take the reference that we get by default. */
        ubik_assert(subgraph->refcount == 1);

        n->node.node_type = DAGC_NODE_CONST;
        n->node.id = ctx->next_id++;

        n->as_const.value.graph = subgraph;
        err = ubik_value_new(&n->as_const.type);
        if (err != OK)
                return err;
        n->as_const.type->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
        /* TODO: lambda type here. */

        return OK;
}

no_ignore ubik_error
ubik_assign_nodes(
        struct ubik_assign_context *ctx,
        struct ubik_graph_builder *builder,
        struct ubik_ast_expr *expr)
{
        union ubik_dagc_any_node *n;
        ubik_error err;

        ubik_alloc1(&n, union ubik_dagc_any_node, ctx->region);

        switch (expr->expr_type)
        {
        case EXPR_ATOM:
                err = _assign_atom_node(ctx, n, expr);
                if (err != OK)
                        return err;
                break;

        case EXPR_APPLY:
                err = ubik_assign_nodes(ctx, builder, expr->apply.head);
                if (err != OK)
                        return err;

                err = ubik_assign_nodes(ctx, builder, expr->apply.tail);
                if (err != OK)
                        return err;

                err = _assign_apply_node(ctx, n, expr);
                if (err != OK)
                        return err;
                break;

        case EXPR_LAMBDA:
                err = _assign_lambda(ctx, n, expr);
                if (err != OK)
                        return err;
                break;

        case EXPR_BLOCK:
                err = _assign_block(ctx, n, builder, expr);
                if (err != OK)
                        return err;
                break;

        case EXPR_COND_BLOCK:
                switch (expr->cond_block.block_type)
                {
                case COND_PREDICATE:
                        err = _assign_pred_block(ctx, n, builder, expr);
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

        err = ubik_bdagc_push_node(builder, &n->node);
        if (err != OK)
                return err;
        expr->gen = &n->node;

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
