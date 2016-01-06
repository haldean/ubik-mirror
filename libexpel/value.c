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
#include <stdlib.h>
#include <string.h>

#include "expel/expel.h"
#include "expel/util.h"
#include "expel/value.h"

no_ignore word_t
xl_read_packed(char **dest, size_t *n, struct xl_value *src)
{
        size_t i, n_bytes, n_copy;
        word_t p;
        struct xl_value *v;

        n_bytes = src->left.v;
        *n = n_bytes;
        *dest = calloc(n_bytes, sizeof(char));
        if (*dest == NULL)
                return ERR_NO_MEMORY;

        i = 0;
        v = src->right.p;
        while (n_bytes)
        {
                n_copy = size_min(n_bytes, 4);
                p = htole64(v->left.v);
                memcpy(&dest[i], &p, n_copy);
                n_bytes -= n_copy;
                i += n_copy;
        }

        return OK;
}

no_ignore word_t
xl_read_string(wchar_t **dest, size_t *n, struct xl_value *src)
{
        word_t err;
        size_t str_size, buf_size;
        char *buf, *orig_buf;

        err = xl_read_packed(&buf, &buf_size, src);
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
        if (*dest == NULL)
        {
                free(*dest);
                return ERR_NO_MEMORY;
        }
        *n = str_size;
        return OK;
}