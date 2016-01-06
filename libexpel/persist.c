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

#include "expel/expel.h"
#include "expel/stream.h"
#include "expel/util.h"

#include <arpa/inet.h>
#include <stdlib.h>
#include <string.h>

#define CURRENT_ENCODING_VERSION 1

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
                out->left.p = calloc(1, sizeof(struct xl_value));
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
                out->right.p = calloc(1, sizeof(struct xl_value));
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

word_t
xl_load(struct xl_dagc *graph, struct xl_stream *sp)
{
        #define READ_INTO(x) { \
                read = xl_stream_read(&x, sp, sizeof(x)); \
                if (read != sizeof(x)) return ERR_NO_DATA; }

        char header[4];
        uint32_t version;
        word_t n_nodes;
        size_t read;
        size_t i;

        READ_INTO(header);
        if (strncmp(header, "expl", 4) != 0)
                return ERR_BAD_HEADER;

        READ_INTO(version);
        version = ntohl(version);
        if (version != CURRENT_ENCODING_VERSION)
                return ERR_UNSUPPORTED_VERSION;

        READ_INTO(n_nodes);
        n_nodes = ntohw(n_nodes);
        graph->n = n_nodes;
        graph->nodes = calloc(n_nodes, sizeof(struct xl_dagc_node));

        for (i = 0; i < n_nodes; i++)
        {
        }
        return OK;
}
