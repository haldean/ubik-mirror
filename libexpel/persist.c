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

        READ_INTO(tag, sp);
        xl_assert((tag & TAG_LEFT_WORD) || (tag & TAG_LEFT_NODE));
        xl_assert((tag & TAG_RIGHT_WORD) || (tag & TAG_RIGHT_NODE));
        xl_assert((tag & 0xF0) == 0);

        out->tag = tag;

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
                        return err;
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
                        return err;
                out->right.p->refcount = 1;
        }

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
__load_apply(struct xl_dagc_apply **node, struct xl_stream *sp)
{
        uint64_t node_index;

        *node = calloc(1, sizeof(struct xl_dagc_apply));
        if (*node == NULL)
                return xl_raise(ERR_NO_MEMORY, "apply alloc");

        (*node)->head.value_type = DAGC_TYPE_UNKNOWN;

        /* Because of the check to make sure that the number of nodes does not
         * exceed the size of a pointer inside xl_load, we can safely jam this
         * index into a pointer. */
        READ_INTO(node_index, sp);
        node_index = ntohw(node_index);
        (*node)->func = (struct xl_dagc_node *) node_index;

        READ_INTO(node_index, sp);
        node_index = ntohw(node_index);
        (*node)->arg = (struct xl_dagc_node *) node_index;

        return OK;
}


no_ignore static xl_error_t
__load_const(struct xl_dagc_const **node, struct xl_stream *sp)
{
        word_t graph_index;
        xl_error_t err;

        *node = calloc(1, sizeof(struct xl_dagc_const));
        if (*node == NULL)
                return xl_raise(ERR_NO_MEMORY, "const alloc");

        READ_INTO((*node)->head.value_type, sp);

        err = xl_new(&(*node)->type);
        if (err != OK)
                return err;
        err = xl_load_value((*node)->type, sp);

        if ((*node)->head.value_type == DAGC_TYPE_VALUE)
        {
                err = xl_new(&(*node)->value.tree);
                if (err != OK)
                        return err;
                err = xl_load_value((*node)->value.tree, sp);
                return err;
        }
        if ((*node)->head.value_type == DAGC_TYPE_GRAPH)
        {
                READ_INTO(graph_index, sp);
                graph_index = ntohw(graph_index);
                (*node)->value.graph = (struct xl_dagc *) graph_index;
                return OK;
        }
        return xl_raise(ERR_BAD_HEADER, "const subtype");
}

no_ignore static xl_error_t
__load_load(struct xl_dagc_load **node, struct xl_stream *sp)
{
        xl_error_t err;
        uint64_t node_index;
        struct xl_value *uri_val;

        *node = calloc(1, sizeof(struct xl_dagc_load));
        if (*node == NULL)
                return xl_raise(ERR_NO_MEMORY, "load alloc");

        (*node)->head.value_type = DAGC_TYPE_UNKNOWN;

        READ_INTO(node_index, sp);
        node_index = ntohw(node_index);
        (*node)->dependent_store = (struct xl_dagc_node *) node_index;

        err = xl_new(&uri_val);
        if (err != OK)
                return err;
        err = xl_load_value(uri_val, sp);
        if (err != OK)
                return err;

        (*node)->loc = calloc(1, sizeof(struct xl_uri));
        err = xl_uri_from_value((*node)->loc, uri_val);
        if (err != OK)
                return err;

        err = xl_release(uri_val);
        return err;
}

no_ignore static xl_error_t
__load_store(struct xl_dagc_store **node, struct xl_stream *sp)
{
        xl_error_t err;
        uint64_t node_index;
        struct xl_value *uri_val;

        *node = calloc(1, sizeof(struct xl_dagc_store));
        if (*node == NULL)
                return xl_raise(ERR_NO_MEMORY, "store alloc");

        (*node)->head.value_type = DAGC_TYPE_UNKNOWN;

        READ_INTO(node_index, sp);
        node_index = ntohw(node_index);
        (*node)->value = (struct xl_dagc_node *) node_index;

        err = xl_new(&uri_val);
        if (err != OK)
                return err;
        err = xl_load_value(uri_val, sp);
        if (err != OK)
                return err;

        (*node)->loc = calloc(1, sizeof(struct xl_uri));
        err = xl_uri_from_value((*node)->loc, uri_val);
        if (err != OK)
                return err;

        err = xl_release(uri_val);
        return err;
}

no_ignore static xl_error_t
__load_input(struct xl_dagc_input **node, struct xl_stream *sp)
{
        *node = calloc(1, sizeof(struct xl_dagc_input));
        if (*node == NULL)
                return xl_raise(ERR_NO_MEMORY, "input alloc");

        READ_INTO((*node)->arg_num, sp);
        return OK;
}

no_ignore static xl_error_t
__load_node(struct xl_dagc_node **node, struct xl_stream *sp)
{
        xl_error_t err;
        word_t node_type;
        uint8_t terminal;

        READ_INTO(node_type, sp);
        READ_INTO(terminal, sp);

        if (xl_stream_drop(sp, 3) != 3)
                return xl_raise(ERR_NO_DATA, "node padding bytes");

        switch (node_type)
        {
        case DAGC_NODE_APPLY:
                err = __load_apply((struct xl_dagc_apply **) node, sp);
                break;
        case DAGC_NODE_CONST:
                err = __load_const((struct xl_dagc_const **) node, sp);
                break;
        case DAGC_NODE_LOAD:
                err = __load_load((struct xl_dagc_load **) node, sp);
                break;
        case DAGC_NODE_STORE:
                err = __load_store((struct xl_dagc_store **) node, sp);
                break;
        case DAGC_NODE_INPUT:
                err = __load_input((struct xl_dagc_input **) node, sp);
                break;
        default:
                return xl_raise(ERR_UNKNOWN_TYPE, "load node");
        }

        if (err != OK)
                return err;
        (*node)->node_type = node_type;
        (*node)->is_terminal = terminal;
        (*node)->flags = 0;
        return OK;
}

no_ignore static xl_error_t
__set_node_pointers(
        struct xl_dagc_node *node,
        struct xl_dagc_node **all_nodes,
        size_t n_nodes)
{
        struct xl_dagc_apply *apply;
        struct xl_dagc_load *load;
        struct xl_dagc_store *store;

        switch (node->node_type)
        {
        case DAGC_NODE_APPLY:
                apply = (struct xl_dagc_apply *) node;
                if ((uintptr_t) apply->func >= n_nodes)
                        return xl_raise(ERR_OUT_OF_BOUNDS, "apply func idx");
                if ((uintptr_t) apply->arg >= n_nodes)
                        return xl_raise(ERR_OUT_OF_BOUNDS, "apply arg idx");

                apply->func = all_nodes[(uintptr_t) apply->func];
                apply->arg = all_nodes[(uintptr_t) apply->arg];
                break;

        case DAGC_NODE_LOAD:
                load = (struct xl_dagc_load *) node;
                if ((uintptr_t) load->dependent_store == UINTPTR_MAX)
                {
                        load->dependent_store = NULL;
                        break;
                }
                if ((uintptr_t) load->dependent_store >= n_nodes)
                        return xl_raise(ERR_OUT_OF_BOUNDS, "load depstore idx");
                load->dependent_store =
                        all_nodes[(uintptr_t) load->dependent_store];
                break;

        case DAGC_NODE_STORE:
                store = (struct xl_dagc_store *) node;
                if ((uintptr_t) store->value >= n_nodes)
                        return xl_raise(ERR_OUT_OF_BOUNDS, "store value idx");

                store->value = all_nodes[(uintptr_t) store->value];
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
__set_graph_pointers(
        struct xl_dagc *graph,
        struct xl_dagc *all_graphs,
        size_t n_graphs)
{
        struct xl_dagc_const *n;
        size_t graph_i;
        size_t i;

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
                n->value.graph = &all_graphs[graph_i];
        }

        return OK;
}

no_ignore static xl_error_t
__load_graph(struct xl_dagc *graph, struct xl_stream *sp)
{
        word_t n_nodes;
        size_t i;
        xl_error_t err;

        READ_INTO(n_nodes, sp);
        n_nodes = ntohw(n_nodes);

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

        graph->n = n_nodes;
        graph->nodes = calloc(n_nodes, sizeof(struct xl_dagc_node *));

        /* First we go through and read all available node information; when
         * this process is done everything has been loaded from the files but
         * inter-node references are still indeces, not pointers. During this
         * process we don't know what the pointers to the dependent nodes will
         * be, because the allocation sizes for the various node types are
         * different. */
        for (i = 0; i < n_nodes; i++)
        {
                err = __load_node(&graph->nodes[i], sp);
                if (err != OK)
                        return err;
        }

        /* Now replace indeces with pointers; now that we've loaded all of them
         * we know what the appropriate pointers are. */
        for (i = 0; i < n_nodes; i++)
        {
                err = __set_node_pointers(
                        graph->nodes[i], graph->nodes, n_nodes);
                if (err != OK)
                        return err;
        }

        return xl_dagc_init(graph);
}

no_ignore xl_error_t
xl_load(struct xl_dagc **graphs, size_t *n_graphs, struct xl_stream *sp)
{

        char header[4];
        uint32_t version;
        word_t n;
        xl_error_t err;
        size_t i;

        READ_INTO(header, sp);
        if (strncmp(header, "expl", 4) != 0)
                return xl_raise(ERR_BAD_HEADER, NULL);

        READ_INTO(version, sp);
        version = ntohl(version);
        if (version != CURRENT_ENCODING_VERSION)
                return xl_raise(ERR_UNSUPPORTED_VERSION, NULL);

        READ_INTO(n, sp);
        n = ntohw(n);
        *n_graphs = n;
        *graphs = calloc(n, sizeof(struct xl_dagc));

        for (i = 0; i < n; i++)
        {
                err = __load_graph(&(*graphs)[i], sp);
                if (err != OK)
                        return err;
        }
        for (i = 0; i < n; i++)
        {
                err = __set_graph_pointers(&(*graphs)[i], *graphs, n);
                if (err != OK)
                        return err;
        }

        return OK;
}
