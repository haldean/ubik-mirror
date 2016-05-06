/*
 * value.c: encoding and decoding ubik_values
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

#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ubik/ubik.h"
#include "ubik/stream.h"
#include "ubik/util.h"
#include "ubik/value.h"

bool
ubik_value_eq(struct ubik_value *v1, struct ubik_value *v2)
{
        if (v1->tag != v2->tag)
                return false;
        if (v1->tag & TAG_LEFT_WORD)
        {
                if (v1->left.w != v2->left.w)
                        return false;
        }
        else
        {
                if (!ubik_value_eq(v1->left.t, v2->left.t))
                        return false;
        }

        if (v1->tag & TAG_RIGHT_WORD)
        {
                if (v1->right.w != v2->right.w)
                        return false;
        }
        else
        {
                if (!ubik_value_eq(v1->right.t, v2->right.t))
                        return false;
        }
        return true;
}

no_ignore ubik_error
ubik_packed_read(uint8_t **dest, size_t *n, struct ubik_value *src)
{
        size_t i, n_bytes, n_copy;
        ubik_word p;
        struct ubik_value *v;
        uint8_t *res;

        if (src->tag != (TAG_VALUE | TAG_LEFT_WORD | TAG_RIGHT_NODE))
                return ubik_raise(ERR_BAD_TAG, "bad tag for packed root");

        n_bytes = src->left.w;
        *n = n_bytes;
        if (n_bytes == 0)
        {
                *dest = NULL;
                return OK;
        }

        res = calloc(n_bytes + 1, sizeof(uint8_t));
        if (res == NULL)
                return ubik_raise(ERR_NO_MEMORY, "read packed");

        i = 0;
        v = src->right.t;
        while (n_bytes)
        {
                if (!(v->tag & TAG_LEFT_WORD))
                        return ubik_raise(ERR_BAD_TAG, "packed val has left node");
                n_copy = size_min(n_bytes, 8);
                p = htonw(v->left.w);
                memcpy(&res[i], &p, n_copy);
                n_bytes -= n_copy;
                i += n_copy;
                if (!(v->tag & TAG_RIGHT_NODE))
                        break;
                v = v->right.t;
        }

        if (n_bytes)
        {
                free(res);
                return ubik_raise(ERR_BAD_VALUE, "not enough data in tree");
        }
        *dest = res;
        return OK;
}

no_ignore ubik_error
ubik_string_read(char **dest, size_t *n, struct ubik_value *src)
{
        ubik_error err;
        err = ubik_packed_read((uint8_t **) dest, n, src);
        if (err != OK)
                return err;
        return OK;
}

no_ignore ubik_error
ubik_value_pack_string(struct ubik_value *dest, char *src, size_t n)
{
        size_t i, t;
        struct ubik_value *v;
        ubik_error err;
        char buf[8];

        dest->tag |= TAG_LEFT_WORD | TAG_RIGHT_NODE;
        dest->left.w = n;

        if (n == 0)
        {
                err = ubik_value_new(&dest->right.t);
                if (err != OK)
                        return err;
                dest->right.t->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
                dest->right.t->left.w = 0;
                dest->right.t->right.w = 0;
                return OK;
        }

        v = dest;
        for (i = 0; i < n; i += 8)
        {
                err = ubik_value_new(&v->right.t);
                if (err != OK)
                        return err;
                v = v->right.t;
                v->tag |= TAG_LEFT_WORD | TAG_RIGHT_NODE;

                t = size_min(n - i, 8);
                if (t < 8)
                        bzero(buf, 8);
                memcpy(buf, &src[i], t);

                v->left.w = htonw(*((ubik_word *) buf));
        }

        v->tag &= ~TAG_RIGHT_NODE;
        v->tag |= TAG_RIGHT_WORD;
        v->right.w = 0;

        return OK;
}

static ubik_error
_print_ascii_word(struct ubik_stream *out, ubik_word word)
{
        size_t written;
        size_t buflen;
        size_t i;
        char *buf;

        buf = ubik_word_explain(word);
        buflen = strlen(buf);
        for (i = 0; i < buflen; i++)
                if (buf[i] < ' ' || buf[i] > 'z')
                        return OK;
        written = ubik_stream_write(out, buf, buflen);
        if (written != buflen)
                return ubik_raise(ERR_WRITE_FAILED, "print value");
        free(buf);
        return OK;
}

no_ignore ubik_error
ubik_value_print(struct ubik_stream *out, struct ubik_value *v)
{
        size_t written;
        size_t n;
        char buf[64];
        ubik_error err;

        if ((v->tag & TAG_TYPE_MASK) != TAG_VALUE)
                return ubik_raise(ERR_BAD_TAG, "print value: not a value");

        buf[0] = '(';
        written = ubik_stream_write(out, buf, 1);
        if (written != 1)
                return ubik_raise(ERR_WRITE_FAILED, "print value");

        if (v->tag & TAG_LEFT_WORD)
        {
                n = snprintf(buf, 64, "0x%" PRIX64 "/", v->left.w);
                written = ubik_stream_write(out, buf, n);
                if (written != n)
                        return ubik_raise(ERR_WRITE_FAILED, "print value");
                err = _print_ascii_word(out, v->left.w);
                if (err != OK)
                        return err;
        }
        else if (v->tag & TAG_LEFT_NODE)
        {
                err = ubik_value_print(out, v->left.t);
                if (err != OK)
                        return err;
        }
        else return ubik_raise(ERR_BAD_TAG, "print value");

        buf[0] = ','; buf[1] = ' ';
        written = ubik_stream_write(out, buf, 2);
        if (written != 2)
                return ubik_raise(ERR_WRITE_FAILED, "print value");

        if (v->tag & TAG_RIGHT_WORD)
        {
                n = snprintf(buf, 64, "0x%" PRIX64 "/", v->right.w);
                written = ubik_stream_write(out, buf, n);
                if (written != n)
                        return ubik_raise(ERR_WRITE_FAILED, "print value");
                err = _print_ascii_word(out, v->right.w);
                if (err != OK)
                        return err;
        }
        else if (v->tag & TAG_RIGHT_NODE)
        {
                err = ubik_value_print(out, v->right.t);
                if (err != OK)
                        return err;
        }
        else return ubik_raise(ERR_BAD_TAG, "print value");

        buf[0] = ')';
        written = ubik_stream_write(out, buf, 1);
        if (written != 1)
                return ubik_raise(ERR_WRITE_FAILED, "print value");
        return OK;
}

no_ignore ubik_error
ubik_value_as_bool(bool *res, struct ubik_value *v)
{
        ubik_word left;
        if (!(v->tag & TAG_LEFT_WORD))
                return ubik_raise(ERR_BAD_TYPE,
                                "value cannot be interpreted as a boolean");
        left = v->left.w;
        if (left != 0 && left != 1)
                return ubik_raise(ERR_BAD_VALUE,
                                "boolean value is not zero or one");
        *res = left == 1;
        return OK;
}