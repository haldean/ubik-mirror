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

#include <stdlib.h>

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
xl_load(struct xl_value *out, struct xl_stream *sp)
{
        tag_t tag;
        word_t ret;

        if (xl_stream_read(&tag, sp, sizeof(tag_t)) != sizeof(tag_t))
                return ERR_NO_DATA;

        if (tag & TAG_LEFT_WORD)
        {
                if (xl_stream_read(&out->left.v, sp, sizeof(word_t)) != sizeof(word_t))
                        return ERR_NO_DATA;
        }
        else
        {
                out->left.p = calloc(1, sizeof(struct xl_value));
                ret = xl_load(out->left.p, sp);
                if (ret)
                        return ret;
        }

        if (tag & TAG_RIGHT_WORD)
        {
                if (xl_stream_read(&out->right.v, sp, sizeof(word_t)) != sizeof(word_t))
                        return ERR_NO_DATA;
        }
        else
        {
                out->right.p = calloc(1, sizeof(struct xl_value));
                ret = xl_load(out->right.p, sp);
                if (ret)
                        return ret;
        }

        return 0;
}

word_t
xl_save(struct xl_stream *sp, struct xl_value *in)
{
        word_t ret;

        if (xl_stream_write(sp, &in->tag, sizeof(tag_t)) != sizeof(tag_t))
                return ERR_WRITE_FAILED;

        if (in->tag & TAG_LEFT_WORD)
        {
                if (xl_stream_write(sp, &in->left.v, sizeof(word_t)) != sizeof(word_t))
                        return ERR_WRITE_FAILED;
        }
        else
        {
                ret = xl_save(sp, in->left.p);
                if (ret)
                        return ret;
        }

        if (in->tag & TAG_RIGHT_WORD)
        {
                if (xl_stream_write(sp, &in->right.v, sizeof(word_t)) != sizeof(word_t))
                        return ERR_WRITE_FAILED;
        }
        else
        {
                ret = xl_save(sp, in->right.p);
                if (ret)
                        return ret;
        }

        return 0;
}
