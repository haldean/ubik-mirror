/*
 * persist.c: save and load expel trees
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

#include "expel/assert.h"
#include "expel/dagc.h"
#include "expel/env.h"
#include "expel/expel.h"
#include "expel/stream.h"
#include "expel/util.h"

#include <arpa/inet.h>
#include <stdlib.h>
#include <string.h>

#define CURRENT_ENCODING_VERSION 1

/* Reads sizeof(x) bytes into x from sp. */
#define READ_INTO(x, sp) \
        if (xl_stream_read(&x, sp, sizeof(x)) != sizeof(x)) \
                return xl_raise(ERR_NO_DATA, #x);

/*
 * Trees have a single binary storage format that is used for network
 * operations and for on-disk storage. The format begins with a header, and
 * then contains a single encoded tree.
 *
 * The header has the following fields:
 *
 *         Byte index   Field
 *                0-4   The constant bytes 0x65 0x78 0x70 0x6C ("expl")
 *                5-8   The version number of the encoding format, as a
 *                      little-endian 4-byte unsigned integer.
 *
 * The tree encoding then proceeds as follows: Begin with the root of the
 * tree to be encoded. The node's 8-bit tag is written to the stream,
 * followed by the encoding of its left child, then its right child. If a
 * child is a value, the value is written to the stream in little-endian
 * format as an 8-byte integer. If a child is a node, recurse and apply
 * this same operation.
 *
 * Tree decoding proceeds in much the same way: Take a byte off of the head
 * of the stream. If the byte indicates the left is a node, recurse. If it
 * indicates the left is a value, read the value as a little-endian 8-byte
 * integer. Repeat the same operation for the right node.
 */

no_ignore xl_error_t
xl_load_value(struct xl_value *out, struct xl_stream *sp)
{
        tag_t tag;
        xl_error_t err;
        xl_error_t err_ignore;

        READ_INTO(tag, sp);
        xl_assert(!(tag & TAG_LEFT_WORD) ^ !(tag & TAG_LEFT_NODE));
        xl_assert(!(tag & TAG_RIGHT_WORD) ^ !(tag & TAG_RIGHT_NODE));
        xl_assert((tag & TAG_TYPE_MASK) == TAG_VALUE);

        if (tag & TAG_LEFT_WORD)
        {
                READ_INTO(out->left.v, sp);
                out->left.v = ntohw(out->left.v);
        }
        else
        {
                err = xl_new(&out->left.p);
                if (err != OK)
                        return err;
                err = xl_load_value(out->left.p, sp);
                if (err != OK)
                {
                        err_ignore = xl_release(out->left.p);
                        unused(err_ignore);

                        /* unset the tag so that releasing the provided node
                         * doesn't dereference a freed pointer */
                        out->tag &= ~TAG_LEFT_NODE;
                        return err;
                }
                out->left.p->refcount = 1;
        }

        if (tag & TAG_RIGHT_WORD)
        {
                READ_INTO(out->right.v, sp);
                out->right.v = ntohw(out->right.v);
        }
        else
        {
                err = xl_new(&out->right.p);
                if (err != OK)
                        return err;
                err = xl_load_value(out->right.p, sp);
                if (err != OK)
                {
                        err_ignore = xl_release(out->right.p);
                        unused(err_ignore);

                        /* unset the tag so that releasing the provided node
                         * doesn't dereference a freed pointer */
                        out->tag &= ~TAG_RIGHT_NODE;
                        return err;
                }
                out->right.p->refcount = 1;
        }

        /* set the tag only if everything went according to plan. This means
         * that callers can safely pass the value here into xl_release, and it
         * won't try to free children that were never populated when we exit
         * early on an error condition. */
        out->tag = tag;
        return OK;
}

no_ignore xl_error_t
xl_save_value(struct xl_stream *sp, struct xl_value *in)
{
        word_t val;
        xl_error_t err;

        if (xl_stream_write(sp, &in->tag, sizeof(tag_t)) != sizeof(tag_t))
                return xl_raise(ERR_WRITE_FAILED, "value tag");

        if (in->tag & TAG_LEFT_WORD)
        {
                val = htonw(in->left.v);
                if (xl_stream_write(sp, &val, sizeof(word_t)) != sizeof(word_t))
                        return xl_raise(ERR_WRITE_FAILED, "left value");
        }
        else
        {
                err = xl_save_value(sp, in->left.p);
                if (err)
                        return err;
        }

        if (in->tag & TAG_RIGHT_WORD)
        {
                val = htonw(in->right.v);
                if (xl_stream_write(sp, &val, sizeof(word_t)) != sizeof(word_t))
                        return xl_raise(ERR_WRITE_FAILED, "right value");
        }
        else
        {
                err = xl_save_value(sp, in->right.p);
                if (err)
                        return err;
        }

        return OK;
}

no_ignore static xl_error_t
_load_apply(struct xl_dagc_apply *node, struct xl_stream *sp)
{
        uint64_t node_index;

        node->head.value_type = DAGC_TYPE_UNKNOWN;

        /* Because of the check to make sure that the number of nodes does not
         * exceed the size of a pointer inside xl_load, we can safely jam this
         * index into a pointer. */
        READ_INTO(node_index, sp);
        node_index = ntohw(node_index);
        node->func = (struct xl_dagc_node *) node_index;

        READ_INTO(node_index, sp);
        node_index = ntohw(node_index);
        node->arg = (struct xl_dagc_node *) node_index;

        return OK;
}


no_ignore static xl_error_t
_load_const(
        struct xl_dagc_const *node,
        struct xl_stream *sp,
        struct xl_value **values,
        size_t n_values)
{
        word_t graph_index;
        word_t value_index;
        word_t value_type;
        xl_error_t err;

        READ_INTO(value_type, sp);
        node->head.value_type = ntohw(value_type);

        READ_INTO(value_index, sp);
        value_index = ntohw(value_index);
        if (value_index >= n_values)
                return xl_raise(ERR_OUT_OF_BOUNDS, "const value index");
        node->type = values[value_index];
        err = xl_take(node->type);
        if (err != OK)
                return err;

        if (node->head.value_type == DAGC_TYPE_VALUE)
        {
                READ_INTO(value_index, sp);
                value_index = ntohw(value_index);
                if (value_index >= n_values)
                        return xl_raise(ERR_OUT_OF_BOUNDS, "const value index");
                node->value.tree = values[value_index];
                err = xl_take(node->value.tree);
                return err;
        }
        if (node->head.value_type == DAGC_TYPE_GRAPH)
        {
                READ_INTO(graph_index, sp);
                graph_index = ntohw(graph_index);
                node->value.graph = (struct xl_dagc *) graph_index;
                return OK;
        }
        return xl_raise(ERR_BAD_HEADER, "const subtype");
}

no_ignore static xl_error_t
_load_load(
        struct xl_dagc_load *node,
        struct xl_stream *sp,
        struct xl_value **values,
        size_t n_values)
{
        xl_error_t err;
        word_t value_index;
        word_t node_index;
        struct xl_value *uri_val;

        node->head.value_type = DAGC_TYPE_UNKNOWN;

        READ_INTO(node_index, sp);
        node_index = ntohw(node_index);
        node->dependent_store = (struct xl_dagc_node *) node_index;

        READ_INTO(value_index, sp);
        value_index = ntohw(value_index);
        if (value_index >= n_values)
                return xl_raise(ERR_OUT_OF_BOUNDS, "load value index");
        uri_val = values[value_index];

        node->loc = calloc(1, sizeof(struct xl_uri));
        err = xl_uri_from_value(node->loc, uri_val);
        if (err != OK)
                return err;

        err = xl_take(node->loc);
        if (err != OK)
                return err;

        return err;
}

no_ignore static xl_error_t
_load_store(
        struct xl_dagc_store *node,
        struct xl_stream *sp,
        struct xl_value **values,
        size_t n_values)
{
        xl_error_t err;
        word_t value_index;
        word_t node_index;
        struct xl_value *uri_val;

        node->head.value_type = DAGC_TYPE_UNKNOWN;

        READ_INTO(node_index, sp);
        node_index = ntohw(node_index);
        node->value = (struct xl_dagc_node *) node_index;

        READ_INTO(value_index, sp);
        value_index = ntohw(value_index);
        if (value_index >= n_values)
                return xl_raise(ERR_OUT_OF_BOUNDS, "store value index");
        uri_val = values[value_index];

        node->loc = calloc(1, sizeof(struct xl_uri));
        err = xl_uri_from_value(node->loc, uri_val);
        if (err != OK)
                return err;

        err = xl_take(node->loc);
        if (err != OK)
                return err;

        return err;
}

no_ignore static xl_error_t
_load_input(
        struct xl_dagc_input *node,
        struct xl_stream *sp,
        struct xl_value **values,
        size_t n_values)
{
        word_t value_index;
        word_t arg_num;
        xl_error_t err;

        READ_INTO(arg_num, sp);
        node->arg_num = ntohw(arg_num);

        READ_INTO(value_index, sp);
        value_index = ntohw(value_index);
        if (value_index >= n_values)
                return xl_raise(ERR_OUT_OF_BOUNDS, "input value index");
        node->required_type = values[value_index];
        err = xl_take(node->required_type);
        return err;
}

no_ignore static xl_error_t
_load_cond(struct xl_dagc_cond *node, struct xl_stream *sp)
{
        uint64_t node_index;

        node->head.value_type = DAGC_TYPE_UNKNOWN;

        READ_INTO(node_index, sp);
        node_index = ntohw(node_index);
        node->condition = (struct xl_dagc_node *) node_index;

        READ_INTO(node_index, sp);
        node_index = ntohw(node_index);
        node->if_true = (struct xl_dagc_node *) node_index;

        READ_INTO(node_index, sp);
        node_index = ntohw(node_index);
        node->if_false = (struct xl_dagc_node *) node_index;

        return OK;
}

no_ignore static xl_error_t
_load_node(
        struct xl_dagc_node *node,
        struct xl_stream *sp,
        struct xl_value **values,
        size_t n_values)
{
        xl_error_t err;
        word_t node_type;
        uint8_t terminal;
        union xl_dagc_any_node *n;
        n = (union xl_dagc_any_node *) node;

        READ_INTO(node_type, sp);
        node_type = ntohw(node_type);

        READ_INTO(terminal, sp);

        if (xl_stream_drop(sp, 3) != 3)
                return xl_raise(ERR_NO_DATA, "node padding bytes");

        switch (node_type)
        {
        case DAGC_NODE_APPLY:
                err = _load_apply(&n->as_apply, sp);
                break;
        case DAGC_NODE_CONST:
                err = _load_const(&n->as_const, sp, values, n_values);
                break;
        case DAGC_NODE_LOAD:
                err = _load_load(&n->as_load, sp, values, n_values);
                break;
        case DAGC_NODE_STORE:
                err = _load_store(&n->as_store, sp, values, n_values);
                break;
        case DAGC_NODE_INPUT:
                err = _load_input(&n->as_input, sp, values, n_values);
                break;
        case DAGC_NODE_COND:
                err = _load_cond(&n->as_cond, sp);
                break;
        default:
                return xl_raise(ERR_UNKNOWN_TYPE, "load node");
        }

        if (err != OK)
                return err;
        node->node_type = node_type;
        node->is_terminal = terminal;
        node->flags = 0;
        return OK;
}

no_ignore static xl_error_t
_set_node_pointers(
        struct xl_dagc_node *node,
        struct xl_dagc_node **all_nodes,
        size_t n_nodes)
{
        union xl_dagc_any_node *n;
        n = (union xl_dagc_any_node *) node;

        switch (node->node_type)
        {
        case DAGC_NODE_APPLY:
                if ((uintptr_t) n->as_apply.func >= n_nodes)
                        return xl_raise(ERR_OUT_OF_BOUNDS, "apply func idx");
                if ((uintptr_t) n->as_apply.arg >= n_nodes)
                        return xl_raise(ERR_OUT_OF_BOUNDS, "apply arg idx");

                n->as_apply.func = all_nodes[(uintptr_t) n->as_apply.func];
                n->as_apply.arg = all_nodes[(uintptr_t) n->as_apply.arg];
                break;

        case DAGC_NODE_COND:
                if ((uintptr_t) n->as_cond.condition >= n_nodes)
                        return xl_raise(ERR_OUT_OF_BOUNDS, "cond condition idx");
                if ((uintptr_t) n->as_cond.if_true >= n_nodes)
                        return xl_raise(ERR_OUT_OF_BOUNDS, "cond true idx");
                if ((uintptr_t) n->as_cond.if_false >= n_nodes)
                        return xl_raise(ERR_OUT_OF_BOUNDS, "cond false idx");

                n->as_cond.condition =
                        all_nodes[(uintptr_t) n->as_cond.condition];
                n->as_cond.if_true =
                        all_nodes[(uintptr_t) n->as_cond.if_true];
                n->as_cond.if_false =
                        all_nodes[(uintptr_t) n->as_cond.if_false];
                break;

        case DAGC_NODE_LOAD:
                if ((uintptr_t) n->as_load.dependent_store == UINTPTR_MAX)
                {
                        n->as_load.dependent_store = NULL;
                        break;
                }
                if ((uintptr_t) n->as_load.dependent_store >= n_nodes)
                        return xl_raise(ERR_OUT_OF_BOUNDS, "load depstore idx");
                n->as_load.dependent_store =
                        all_nodes[(uintptr_t) n->as_load.dependent_store];
                break;

        case DAGC_NODE_STORE:
                if ((uintptr_t) n->as_store.value >= n_nodes)
                        return xl_raise(ERR_OUT_OF_BOUNDS, "store value idx");
                n->as_store.value = all_nodes[(uintptr_t) n->as_store.value];
                break;

        case DAGC_NODE_CONST:
        case DAGC_NODE_INPUT:
                break;
        default:
                return xl_raise(ERR_UNKNOWN_TYPE, "set node pointers");
        }
        return OK;
}

no_ignore static xl_error_t
_set_graph_pointers(
        struct xl_dagc *graph,
        struct xl_dagc **all_graphs,
        size_t n_graphs)
{
        struct xl_dagc_const *n;
        size_t graph_i;
        size_t i;
        xl_error_t err;

        for (i = 0; i < graph->n; i++)
        {
                /* C is fun. */
                n = (struct xl_dagc_const *) graph->nodes[i];
                if (n->head.node_type != DAGC_NODE_CONST)
                        continue;
                if (n->head.value_type != DAGC_TYPE_GRAPH)
                        continue;
                graph_i = (uintptr_t) n->value.graph;
                if (graph_i >= n_graphs)
                        return xl_raise(ERR_OUT_OF_BOUNDS, "const graph idx");
                n->value.graph = all_graphs[graph_i];

                err = xl_take(n->value.graph);
                if (err != OK)
                        return err;
        }

        return OK;
}

no_ignore static xl_error_t
_load_graph(
        struct xl_dagc **graph,
        struct xl_stream *sp,
        struct xl_value **values,
        size_t n_values)
{
        word_t n_nodes;
        word_t result_idx;
        size_t i;
        xl_error_t err;

        READ_INTO(n_nodes, sp);
        n_nodes = ntohw(n_nodes);

        if (n_nodes == 0)
                return xl_raise(
                        ERR_BAD_GRAPH, "graphs must have at least 1 node");

        err = xl_new_dagc(graph, n_nodes);
        if (err != OK)
                return err;

        READ_INTO(result_idx, sp);
        result_idx = ntohw(result_idx);

        /* This binary is too large to be read on this machine. Note that
         * performing this check up front means that we don't have to worry
         * later about whether indeces fit into pointers (which is how we hack
         * the single-pass node loading).
         *
         * It's worth mentioning that this almost certainly will not happen
         * except for the most huge binaries; this means that there's more than
         * 4G nodes in the graph, which means this thing is going to need
         * terabytes of memory. Good luck. */
        if (n_nodes > (uint64_t) UINTPTR_MAX)
                return xl_raise(ERR_NO_MEMORY, "n_nodes too big");

        if (result_idx >= n_nodes)
                return xl_raise(ERR_BAD_HEADER, "result_idx > n_nodes");

        /* First we go through and read all available node information; when
         * this process is done everything has been loaded from the files but
         * inter-node references are still indeces, not pointers. During this
         * process we don't know what the pointers to the dependent nodes will
         * be, because the allocation sizes for the various node types are
         * different. */
        for (i = 0; i < n_nodes; i++)
        {
                err = _load_node((*graph)->nodes[i], sp, values, n_values);
                if (err != OK)
                        return err;
        }
        (*graph)->result = (*graph)->nodes[result_idx];
        if (!(*graph)->result->is_terminal)
                return xl_raise(ERR_BAD_GRAPH, "result node is not terminal");

        /* Now replace indeces with pointers; now that we've loaded all of them
         * we know what the appropriate pointers are. */
        for (i = 0; i < n_nodes; i++)
        {
                err = _set_node_pointers(
                        (*graph)->nodes[i], (*graph)->nodes, n_nodes);
                if (err != OK)
                        return err;
        }

        return xl_dagc_init(*graph);
}

no_ignore xl_error_t
xl_load(struct xl_dagc ***graphs, size_t *ret_n_graphs, struct xl_stream *sp)
{

        char header[4];
        uint32_t version;
        struct xl_value **values;
        word_t n_graphs, n_values;
        xl_error_t err;
        size_t i;

        READ_INTO(header, sp);
        if (strncmp(header, "expl", 4) != 0)
        {
                err = xl_raise(ERR_BAD_HEADER, NULL);
                goto error;
        }

        READ_INTO(version, sp);
        version = ntohl(version);
        if (version != CURRENT_ENCODING_VERSION)
        {
                err = xl_raise(ERR_UNSUPPORTED_VERSION, NULL);
                goto error;
        }

        READ_INTO(n_graphs, sp);
        n_graphs = ntohw(n_graphs);
        if (n_graphs == 0)
        {
                err = xl_raise(ERR_BAD_HEADER, "must have at least one graph");
                goto error;
        }

        *ret_n_graphs = n_graphs;
        *graphs = calloc(n_graphs, sizeof(struct xl_dagc *));
        if (*graphs == NULL)
        {
                err = xl_raise(ERR_NO_MEMORY, "graph list alloc");
                goto error;
        }

        READ_INTO(n_values, sp);
        n_values = ntohw(n_values);
        values = calloc(n_values, sizeof(struct xl_value *));
        if (values == NULL)
        {
                err = xl_raise(ERR_NO_MEMORY, "value list alloc");
                goto error;
        }

        for (i = 0; i < n_values; i++)
        {
                err = xl_new(&values[i]);
                if (err != OK)
                        goto error;
                err = xl_load_value(values[i], sp);
                if (err != OK)
                        goto error;
        }
        /* At the end of loading, we own a reference to each value that has been
         * loaded. We then load the graphs and release the values, so only the
         * values owned by the graph stay alive. */

        for (i = 0; i < n_graphs; i++)
        {
                err = _load_graph(&(*graphs)[i], sp, values, n_values);
                if (err != OK)
                        goto error;
        }
        for (i = 0; i < n_graphs; i++)
        {
                err = _set_graph_pointers((*graphs)[i], *graphs, n_graphs);
                if (err != OK)
                        goto error;
        }

        for (i = 0; i < n_values; i++)
        {
                err = xl_release(values[i]);
                if (err != OK)
                        goto error;
        }

        return OK;

error:
        *ret_n_graphs = 0;
        return err;
}
