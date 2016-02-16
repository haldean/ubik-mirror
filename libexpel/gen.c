/*
 * gen.c: expel bytecode generation
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

#include "expel/bdagc.h"
#include "expel/dagc.h"
#include "expel/expel.h"
#include "expel/gen.h"
#include "expel/types.h"

#include <stdlib.h>

no_ignore static xl_error
_assign_atom_node(
        union xl_dagc_any_node *n,
        struct xl_ast_expr *expr)
{
        xl_error err;

        switch (expr->atom->atom_type)
        {
        case ATOM_INT:
                n->node.node_type = DAGC_NODE_CONST;
                /* TODO: node IDs? */
                n->node.id = 0;

                err = xl_value_new(&n->as_const.type);
                if (err != OK)
                        return err;
                err = xl_type_word(n->as_const.type);
                if (err != OK)
                        return err;

                err = xl_value_new(&n->as_const.value.tree);
                if (err != OK)
                        return err;
                n->as_const.value.tree->left.w = expr->atom->integer;
                return OK;

        case ATOM_NUM:
                n->node.node_type = DAGC_NODE_CONST;
                n->node.id = 0;

                err = xl_value_new(&n->as_const.type);
                if (err != OK)
                        return err;
                err = xl_type_float(n->as_const.type);
                if (err != OK)
                        return err;

                err = xl_value_new(&n->as_const.value.tree);
                if (err != OK)
                        return err;
                n->as_const.value.tree->left.f = expr->atom->number;
                return OK;

        case ATOM_NAME:
        case ATOM_TYPE_NAME:
                return xl_raise(ERR_NOT_IMPLEMENTED, "expr name");
        }
        return xl_raise(ERR_UNKNOWN_TYPE, "compile atom type");
}

no_ignore static xl_error
_assign_nodes(
        struct xl_graph_builder *builder,
        struct xl_ast_expr *expr)
{
        union xl_dagc_any_node *n;
        xl_error err;

        n = calloc(1, sizeof(union xl_dagc_any_node));

        switch (expr->expr_type)
        {
        case EXPR_ATOM:
                err = _assign_atom_node(n, expr);
                if (err != OK)
                        return err;
                break;

        default:
                return xl_raise(ERR_UNKNOWN_TYPE, "compile assign node");
        }

        err = xl_bdagc_push_node(builder, &n->node);
        if (err != OK)
                return err;
        expr->gen = &n->node;

        return OK;
}

no_ignore static xl_error
xl_compile_binding(
        struct xl_dagc **graphs,
        size_t n_graphs, 
        struct xl_ast_binding *binding)
{
        struct xl_graph_builder builder;
        xl_error err;

        err = xl_bdagc_init(&builder);
        if (err != OK)
                return err;

        err = _assign_nodes(&builder, binding->expr);
        if (err != OK)
                return err;

        builder.result = binding->expr->gen;
        builder.result->is_terminal = true;

        err = xl_bdagc_build(&graphs[n_graphs], &builder);
        if (err != OK)
                return err;

        return OK;
}

no_ignore xl_error
xl_compile(
        struct xl_dagc ***graphs,
        size_t *n_graphs, 
        struct xl_ast *ast)
{
        size_t i;
        xl_error err;

        *graphs = calloc(ast->n_bindings, sizeof(struct xl_dagc *));
        if (*graphs == NULL)
                return xl_raise(ERR_NO_MEMORY, "compile graph alloc");
        *n_graphs = 0;

        for (i = 0; i < ast->n_bindings; i++)
        {
                err = xl_compile_binding(
                        *graphs, (*n_graphs)++, ast->bindings[i]);
                if (err != OK)
                        return err;
        }

        return OK;
}
