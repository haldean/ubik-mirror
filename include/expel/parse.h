/*
 * parse.h: parser hooks for the Expel language
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

#pragma once

#include "expel/expel.h"


enum xl_token_type
{
        TOKEN_INT,
        TOKEN_FLOAT,
        TOKEN_NAME,
        TOKEN_LPAREN,
        TOKEN_RPAREN,
        MAX_TOKEN,
};

struct xl_token
{
        enum xl_token_type type;
        char *contents;
};

typedef xl_error (*xl_tokenize_cb)(void *state, struct xl_token *t);

no_ignore xl_error
xl_tokenize(xl_tokenize_cb cb, void *state, char *src, size_t len);
