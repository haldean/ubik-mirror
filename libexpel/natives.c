/*
 * natives.c: built-in native methods
 * Copyright (C) 2015, Haldean Brown
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

#include "expel/dagc.h"
#include "expel/env.h"
#include "expel/expel.h"
#include "expel/natives.h"
#include "expel/util.h"

static xl_error_t
__native_unsigned_add(struct xl_dagc *graph)
{
        struct xl_value *res;
        xl_error_t err;

        err = xl_new(&res);
        if (err != OK)
                return err;

        res->left.v = graph->nodes[0]->known.tree->left.v +
                      graph->nodes[1]->known.tree->left.v;
        res->right.v = 0;

        graph->terminals[0]->known.tree = res;
        graph->terminals[0]->known_type = graph->nodes[0]->known_type;
        graph->terminals[0]->value_type = DAGC_TYPE_VALUE;

        return OK;
}

no_ignore static xl_error_t
__register_unsigned_add(struct xl_env *env)
{
        xl_error_t err;

        struct xl_dagc *add_graph;
        struct xl_dagc_native *native_graph;
        struct xl_uri *uri;
        struct xl_value *type;
        struct xl_dagc_input *in;
        union xl_value_or_graph ins;

        add_graph = calloc(1, sizeof(struct xl_dagc_native));
        native_graph = (struct xl_dagc_native *) add_graph;

        add_graph->nodes = calloc(3, sizeof(struct xl_dagc_node *));

        in = calloc(1, sizeof(struct xl_dagc_input));
        in->head.node_type = DAGC_NODE_INPUT;
        in->head.value_type = DAGC_TYPE_UNKNOWN;
        in->head.known_type = NULL;
        in->head.known.any = NULL;
        in->head.is_terminal = 0x00;
        in->head.flags = 0x00;
        in->arg_num = 0;
        err = xl_new(&in->required_type);
        if (err != OK)
                return err;
        add_graph->nodes[0] = (struct xl_dagc_node *) in;

        in = calloc(1, sizeof(struct xl_dagc_input));
        in->head.node_type = DAGC_NODE_INPUT;
        in->head.value_type = DAGC_TYPE_UNKNOWN;
        in->head.known_type = NULL;
        in->head.known.any = NULL;
        in->head.is_terminal = 0x00;
        in->head.flags = 0x00;
        in->arg_num = 1;
        err = xl_new(&in->required_type);
        if (err != OK)
                return err;
        add_graph->nodes[1] = (struct xl_dagc_node *) in;

        add_graph->nodes[2] = calloc(1, sizeof(struct xl_dagc_input));
        add_graph->nodes[2]->node_type = DAGC_NODE_NATIVE;
        add_graph->nodes[2]->value_type = DAGC_TYPE_UNKNOWN;
        add_graph->nodes[2]->known_type = NULL;
        add_graph->nodes[2]->known.any = NULL;
        add_graph->nodes[2]->is_terminal = 0x01;
        add_graph->nodes[2]->flags = 0x00;

        add_graph->n = 3;

        err = xl_dagc_init(add_graph);
        if (err != OK)
                return err;

        err = xl_new(&type);
        if (err != OK)
                return err;
        // TODO: fill in type

        native_graph->evaluator = __native_unsigned_add;

        uri = calloc(1, sizeof(struct xl_uri));
        err = xl_uri_native(uri, L"uadd");
        if (err != OK)
                return err;

        ins.graph = add_graph;
        err = xl_set(env, uri, ins, type, DAGC_TYPE_GRAPH);
        if (err != OK)
                return err;

        err = xl_release(uri);
        if (err != OK)
                return err;
        err = xl_release(type);
        if (err != OK)
                return err;
        err = xl_release(add_graph);
        if (err != OK)
                return err;

        return OK;
}

no_ignore xl_error_t
xl_register_natives(struct xl_env *env)
{
        xl_error_t err;

        err = __register_unsigned_add(env);
        if (err != OK)
                return err;

        return OK;
}
