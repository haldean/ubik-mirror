/*
 * token.h: ubik language tokenizer
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

#include "ubik/ubik.h"
#include "ubik/stream.h"

struct ubik_token
{
        int token_code;
        char *text;
        size_t text_len;
        int line_no;
};

typedef ubik_error (*ubik_token_cb)(struct ubik_token t);

no_ignore ubik_error
ubik_tokenize(ubik_token_cb callback, struct ubik_stream *stream);
