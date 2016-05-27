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
#include "ubik/resolve.h"
#include "ubik/types.h"
#include "ubik/uri.h"
#include "ubik/value.h"

no_ignore static ubik_error
_set_name_in_scope(
        struct ubik_resolve_scope *scope,
        char *name,
        struct ubik_dagc_node *n)
{
        struct ubik_resolve_name *bind;
        size_t i;
        bool found;

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

        case ATOM_TYPE_NAME:
        case ATOM_NAME:
                res_type = expr->atom->name_loc->type;

                if (res_type == RESOLVE_GLOBAL || res_type == RESOLVE_NATIVE)
                {
                        n->node.node_type = DAGC_NODE_LOAD;
                        n->node.id = ctx->next_id++;

                        n->as_load.loc = calloc(1, sizeof(struct ubik_uri));

                        if (res_type == RESOLVE_NATIVE)
                                err = ubik_uri_native(
                                        n->as_load.loc, expr->atom->str);
                        else
                                err = ubik_uri_user(
                                        n->as_load.loc, expr->atom->str);
                        if (err != OK)
                                return err;

                        err = ubik_take(n->as_load.loc);
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

                n->as_load.loc = calloc(1, sizeof(struct ubik_uri));

                err = ubik_uri_package(
                        n->as_load.loc,
                        expr->atom->qualified.head,
                        expr->atom->qualified.tail);
                if (err != OK)
                        return err;

                err = ubik_take(n->as_load.loc);
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
_assign_conditional_node(
        struct ubik_assign_context *ctx,
        union ubik_dagc_any_node *n,
        struct ubik_ast_expr *expr)
{
        n->node.node_type = DAGC_NODE_COND;
        n->node.id = ctx->next_id++;

        n->as_cond.condition = expr->condition.cond->gen;
        n->as_cond.if_true = expr->condition.implied->gen;
        n->as_cond.if_false = expr->condition.opposed->gen;

        return OK;
}

no_ignore static ubik_error
_assign_block(
        union ubik_dagc_any_node *n,
        struct ubik_ast_expr *expr)
{
        unused(n);
        unused(expr);
        return ubik_raise(ERR_NOT_IMPLEMENTED, "block codegen");
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

        err = ubik_bdagc_init(&builder);
        if (err != OK)
                return err;

        ubik_assert(expr->lambda.args != NULL);
        t = expr->lambda.args;
        i = 0;
        while (t->name != NULL)
        {
                input_node = calloc(1, sizeof(struct ubik_dagc_input));
                if (input_node == NULL)
                        return ubik_raise(ERR_NO_MEMORY, "input node alloc");
                input_node->head.node_type = DAGC_NODE_INPUT;
                input_node->head.id = ctx->next_id++;
                input_node->arg_num = i++;

                err = ubik_bdagc_push_node(
                        &builder, (struct ubik_dagc_node *) input_node);
                if (err != OK)
                        return err;
                t->gen = (struct ubik_dagc_node *) input_node;

                err = _set_name_in_scope(expr->scope, t->name, t->gen);
                if (err != OK)
                        return err;

                t = t->next;
        }

        err = ubik_assign_nodes(ctx, &builder, expr->lambda.body);
        if (err != OK)
                return err;

        builder.result = expr->lambda.body->gen;
        builder.result->is_terminal = true;

        err = ubik_bdagc_build(&subgraph, &builder);
        if (err != OK)
                return err;
        subgraph->tag |= TAG_GRAPH_UNRESOLVED;

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

        n = calloc(1, sizeof(union ubik_dagc_any_node));

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

        case EXPR_CONDITIONAL:
                err = ubik_assign_nodes(ctx, builder, expr->condition.cond);
                if (err != OK)
                        return err;

                err = ubik_assign_nodes(ctx, builder, expr->condition.implied);
                if (err != OK)
                        return err;

                err = ubik_assign_nodes(ctx, builder, expr->condition.opposed);
                if (err != OK)
                        return err;

                err = _assign_conditional_node(ctx, n, expr);
                if (err != OK)
                        return err;
                break;

        case EXPR_LAMBDA:
                err = _assign_lambda(ctx, n, expr);
                if (err != OK)
                        return err;
                break;

        case EXPR_BLOCK:
                err = _assign_block(n, expr);
                if (err != OK)
                        return err;
                break;

        case EXPR_CONSTRUCTOR:
        case EXPR_COND_BLOCK:
        default:
                return ubik_raise(ERR_UNKNOWN_TYPE, "compile assign node");
        }

        err = ubik_bdagc_push_node(builder, &n->node);
        if (err != OK)
                return err;
        expr->gen = &n->node;

        return OK;
}

void
ubik_assign_context_free(struct ubik_assign_context *ctx)
{
        unused(ctx);
}
