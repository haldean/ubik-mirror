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
        if (xl_stream_read(&x, sp, sizeof(x)) != sizeof(x)) return ERR_NO_DATA;

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

word_t
xl_load_value(struct xl_value *out, struct xl_stream *sp)
{
        size_t read;
        tag_t tag;
        word_t ret;

        read = xl_stream_read(&tag, sp, sizeof(tag_t));
        if (read != sizeof(tag_t))
                return ERR_NO_DATA;
        out->tag = tag;
        out->refcount = 0;

        if (tag & TAG_LEFT_WORD)
        {
                read = xl_stream_read(&out->left.v, sp, sizeof(word_t));
                if (read != sizeof(word_t))
                        return ERR_NO_DATA;
                out->left.v = ntohw(out->left.v);
        }
        else
        {
                ret = xl_new(&out->left.p);
                if (ret != OK)
                        return ret;
                ret = xl_load_value(out->left.p, sp);
                if (ret != OK)
                        return ret;
                out->left.p->refcount = 1;
        }

        if (tag & TAG_RIGHT_WORD)
        {
                read = xl_stream_read(&out->right.v, sp, sizeof(word_t));
                if (read != sizeof(word_t))
                        return ERR_NO_DATA;
                out->right.v = ntohw(out->right.v);
        }
        else
        {
                ret = xl_new(&out->right.p);
                if (ret != OK)
                        return ret;
                ret = xl_load_value(out->right.p, sp);
                if (ret != OK)
                        return ret;
                out->right.p->refcount = 1;
        }

        return OK;
}

word_t
xl_save_value(struct xl_stream *sp, struct xl_value *in)
{
        word_t val;
        word_t ret;

        if (xl_stream_write(sp, &in->tag, sizeof(tag_t)) != sizeof(tag_t))
                return ERR_WRITE_FAILED;

        if (in->tag & TAG_LEFT_WORD)
        {
                val = htonw(in->left.v);
                if (xl_stream_write(sp, &val, sizeof(word_t)) != sizeof(word_t))
                        return ERR_WRITE_FAILED;
        }
        else
        {
                ret = xl_save_value(sp, in->left.p);
                if (ret)
                        return ret;
        }

        if (in->tag & TAG_RIGHT_WORD)
        {
                val = htonw(in->right.v);
                if (xl_stream_write(sp, &val, sizeof(word_t)) != sizeof(word_t))
                        return ERR_WRITE_FAILED;
        }
        else
        {
                ret = xl_save_value(sp, in->right.p);
                if (ret)
                        return ret;
        }

        return OK;
}

no_ignore static word_t
__load_apply(struct xl_dagc_apply **node, struct xl_stream *sp)
{
        uint64_t node_index;

        *node = calloc(1, sizeof(struct xl_dagc_apply));
        if (*node == NULL)
                return ERR_NO_MEMORY;

        /* Because of the check to make sure that the number of nodes does not
         * exceed the size of a pointer inside xl_load, we can safely jam this
         * index into a pointer. */
        READ_INTO(node_index, sp);
        (*node)->func = (struct xl_dagc_node *) node_index;

        READ_INTO(node_index, sp);
        (*node)->arg = (struct xl_dagc_node *) node_index;

        return OK;
}


no_ignore static word_t
__load_const(struct xl_dagc_const **node, struct xl_stream *sp)
{
        word_t err;

        *node = calloc(1, sizeof(struct xl_dagc_apply));
        if (*node == NULL)
                return ERR_NO_MEMORY;

        err = xl_new(&(*node)->value);
        if (err != OK)
                return err;
        err = xl_load_value((*node)->value, sp);
        if (err != OK)
                return err;

        err = xl_new(&(*node)->type);
        if (err != OK)
                return err;
        err = xl_load_value((*node)->type, sp);
        return err;
}

no_ignore static word_t
__load_load(struct xl_dagc_load **node, struct xl_stream *sp)
{
        word_t err;
        uint64_t node_index;
        struct xl_value *uri_val;

        *node = calloc(1, sizeof(struct xl_dagc_load));
        if (*node == NULL)
                return ERR_NO_MEMORY;

        READ_INTO(node_index, sp);
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

no_ignore static word_t
__load_store(struct xl_dagc_store **node, struct xl_stream *sp)
{
        word_t err;
        uint64_t node_index;
        struct xl_value *uri_val;

        *node = calloc(1, sizeof(struct xl_dagc_store));
        if (*node == NULL)
                return ERR_NO_MEMORY;

        READ_INTO(node_index, sp);
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

no_ignore static word_t
__load_node(struct xl_dagc_node **node, struct xl_stream *sp)
{
        word_t err;
        word_t node_type;
        uint8_t terminal;

        READ_INTO(node_type, sp);
        READ_INTO(terminal, sp);

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
        default:
                return ERR_UNKNOWN_TYPE;
        }

        if (err != OK)
                return err;
        (*node)->node_type = node_type;
        (*node)->is_terminal = terminal;
        (*node)->flags = 0;
        return OK;
}

no_ignore static word_t
__set_node_pointers(struct xl_dagc_node *node, struct xl_dagc_node **all_nodes)
{
        struct xl_dagc_apply *apply;
        struct xl_dagc_load *load;
        struct xl_dagc_store *store;

        switch (node->node_type)
        {
        case DAGC_NODE_APPLY:
                apply = (struct xl_dagc_apply *) node;
                apply->func = all_nodes[(uintptr_t) apply->func];
                apply->arg = all_nodes[(uintptr_t) apply->arg];
                break;
        case DAGC_NODE_LOAD:
                load = (struct xl_dagc_load *) node;
                load->dependent_store =
                        all_nodes[(uintptr_t) load->dependent_store];
                break;
        case DAGC_NODE_STORE:
                store = (struct xl_dagc_store *) node;
                store->value = all_nodes[(uintptr_t) store->value];
                break;
        case DAGC_NODE_CONST:
                break;
        default:
                return ERR_UNKNOWN_TYPE;
        }
        return OK;
}

no_ignore word_t
xl_load(struct xl_dagc *graph, struct xl_stream *sp)
{

        char header[4];
        uint32_t version;
        word_t n_nodes;
        word_t err;
        size_t i;

        READ_INTO(header, sp);
        if (strncmp(header, "expl", 4) != 0)
                return ERR_BAD_HEADER;

        READ_INTO(version, sp);
        version = ntohl(version);
        if (version != CURRENT_ENCODING_VERSION)
                return ERR_UNSUPPORTED_VERSION;

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
                return ERR_NO_MEMORY;

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
                err = __set_node_pointers(graph->nodes[i], graph->nodes);
                if (err != OK)
                        return err;
        }
        return OK;
}
