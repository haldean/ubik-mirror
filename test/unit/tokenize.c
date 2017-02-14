/*
 * tokenize.c: run tests on tokenizer
 * Copyright (C) 2017, Haldean Brown
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

#include "ubik/tokenize.h"
#include "ubik/util.h"
#include "unit.h"

#include <string.h>

static ubik_error
token_cb(struct ubik_token *t, void *ignored)
{
        unused(ignored);
        unused(t);
        return OK;
}

#define parse_literal(lit) do {                                         \
        ubik_stream_reset(&s);                                          \
        assert(ubik_stream_write(&s, lit, strlen(lit)) == strlen(lit)); \
        assert(ubik_tokenize(token_cb, &s, NULL) == OK);                \
} while (0)

test_t
tokenize()
{
        struct ubik_stream s;
        assert(ubik_stream_buffer(&s, NULL) == OK);
        parse_literal("`* ` * [ [   ][ hello]]");
        return ok;
}

run_single(tokenize)
