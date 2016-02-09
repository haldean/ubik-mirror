/*
 * parse.c: parser for the Expel language
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


#include <stdlib.h>

#include "expel/parse.h"


inline static bool
is_separator(char c)
{
        return c == ' ' || c == '\n' || c == '\r';
}


inline static bool
is_name_char(char c)
{
        return ('0' <= c <= '9') ||
               ('a' <= c <= 'z') ||
               ('A' <= c <= 'Z') ||
               c == '-' ||
               c == '/' ||
               c == '\'' ||
               c == '?' ||
               c == '_';
}


no_ignore static xl_error
_emit_token(xl_tokenize_cb cb, void *state, size_t &i, char *src, size_t len)
{
        uint8_t token_ok[MAX_TOKEN];
        size_t start_i;
        struct xl_token *t;

        while (is_separator(src[*i]))
                *i++;

        start_i = i;
        t = calloc(1, sizeof(struct xl_token));

        if (src[*i] == '(')
        {
                t->type = TOKEN_LPAREN;
                t->contents = calloc(2, sizeof(char));
                t->contents[0] = '(';
                *i++;
                return cb(state, t);
        }

        if (src[*i] == ')')
        {
                t->type = TOKEN_RPAREN;
                t->contents = calloc(2, sizeof(char));
                t->contents[0] = ')';
                *i++;
                return cb(state, t);
        }

        return OK;
}


no_ignore xl_error
xl_tokenize(xl_tokenize_cb cb, void *state, char *src, size_t len)
{
        xl_error err;
        size_t i;

        while (i < len)
        {
                err = _emit_token(cb, state, &i, src, len);
                if (err != OK)
                        return err;

                while (is_separator(src[i]) && i < len)
                        i++;
        }

        return OK;
}
