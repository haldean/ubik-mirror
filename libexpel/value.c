/*
 * value.c: encoding and decoding xl_values
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

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "expel/expel.h"
#include "expel/stream.h"
#include "expel/util.h"
#include "expel/value.h"

bool
xl_value_eq(struct xl_value *v1, struct xl_value *v2)
{
        if (v1->tag != v2->tag)
                return false;
        if (v1->tag & TAG_LEFT_WORD)
        {
                if (v1->left.v != v2->left.v)
                        return false;
        }
        else
        {
                if (!xl_value_eq(v1->left.p, v2->left.p))
                        return false;
        }

        if (v1->tag & TAG_RIGHT_WORD)
        {
                if (v1->right.v != v2->right.v)
                        return false;
        }
        else
        {
                if (!xl_value_eq(v1->right.p, v2->right.p))
                        return false;
        }
        return true;
}

no_ignore xl_error_t
xl_read_packed(uint8_t **dest, size_t *n, struct xl_value *src)
{
        size_t i, n_bytes, n_copy;
        word_t p;
        struct xl_value *v;
        uint8_t *res;

        n_bytes = src->left.v;
        *n = n_bytes;
        res = calloc(n_bytes + 1, sizeof(uint8_t));
        if (res == NULL)
                return xl_raise(ERR_NO_MEMORY, "read packed");

        i = 0;
        v = src->right.p;
        while (n_bytes)
        {
                n_copy = size_min(n_bytes, 8);
                p = htonw(v->left.v);
                memcpy(&res[i], &p, n_copy);
                n_bytes -= n_copy;
                i += n_copy;
                v = v->right.p;
        }

        *dest = res;
        return OK;
}

no_ignore xl_error_t
xl_read_string(wchar_t **dest, size_t *n, struct xl_value *src)
{
        xl_error_t err;
        size_t str_size, buf_size;
        char *buf, *orig_buf;

        err = xl_read_packed((uint8_t **) &buf, &buf_size, src);
        if (err != OK)
                return err;

        /* The hilariously-named mbsrtowcs (who the hell abbreviates multi-byte
         * string as mbsr?) has the hilarious behavior of modifying the input
         * string you give it because it hates you. We make a copy of it here so
         * that we can free it when it's done.
         *
         * Seriously, couldn't you just take a size_t * and increment that? */
        orig_buf = buf;

        *dest = calloc(buf_size, sizeof(wchar_t));
        str_size = mbsrtowcs(*dest, (const char **) &buf, buf_size, NULL);
        free(orig_buf);

        *dest = realloc(*dest, str_size * sizeof(wchar_t));
        if (*dest == NULL && str_size > 0)
        {
                free(*dest);
                return xl_raise(ERR_NO_MEMORY, "read string");
        }
        *n = str_size;
        return OK;
}

no_ignore xl_error_t
xl_print_value(struct xl_stream *out, struct xl_value *v)
{
        size_t written;
        size_t n;
        char buf[64];
        xl_error_t err;

        if ((v->tag & TAG_TYPE_MASK) != TAG_VALUE)
                return xl_raise(ERR_BAD_TAG, "print value: not a value");

        buf[0] = '(';
        written = xl_stream_write(out, buf, 1);
        if (written != 1)
                return xl_raise(ERR_WRITE_FAILED, "print value");

        if (v->tag & TAG_LEFT_WORD)
        {
                n = snprintf(buf, 64, "0x%lX", v->left.v);
                written = xl_stream_write(out, buf, n);
                if (written != n)
                        return xl_raise(ERR_WRITE_FAILED, "print value");
        }
        else if (v->tag & TAG_LEFT_NODE)
        {
                err = xl_print_value(out, v->left.p);
                if (err != OK)
                        return err;
        }
        else return xl_raise(ERR_BAD_TAG, "print value");

        buf[0] = ','; buf[1] = ' ';
        written = xl_stream_write(out, buf, 2);
        if (written != 2)
                return xl_raise(ERR_WRITE_FAILED, "print value");

        if (v->tag & TAG_RIGHT_WORD)
        {
                n = snprintf(buf, 64, "0x%lX", v->right.v);
                written = xl_stream_write(out, buf, n);
                if (written != n)
                        return xl_raise(ERR_WRITE_FAILED, "print value");
        }
        else if (v->tag & TAG_RIGHT_NODE)
        {
                err = xl_print_value(out, v->right.p);
                if (err != OK)
                        return err;
        }
        else return xl_raise(ERR_BAD_TAG, "print value");

        buf[0] = ')';
        written = xl_stream_write(out, buf, 1);
        if (written != 1)
                return xl_raise(ERR_WRITE_FAILED, "print value");
        return OK;
}

no_ignore xl_error_t
xl_value_as_bool(bool *res, struct xl_value *v)
{
        word_t left;
        if (!(v->tag & TAG_LEFT_WORD))
                return xl_raise(ERR_BAD_TYPE,
                                "value cannot be interpreted as a boolean");
        left = v->left.v;
        if (left != 0 && left != 1)
                return xl_raise(ERR_BAD_VALUE,
                                "boolean value is not zero or one");
        *res = left == 1;
        return OK;
}
