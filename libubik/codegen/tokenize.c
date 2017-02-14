/*
 * tokenize.c: tokenization of Ubik source
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

#include <string.h>

#define TOKEN_DEBUG 1

char *ubik_token_names[] = {
        [BLOCK_OPEN] = "BLOCK_OPEN",
        [BLOCK_CLOSE] = "BLOCK_CLOSE",
        [NAME] = "NAME",
        [NUMBER] = "NUMBER",
        [START_CLAUSE] = "START_CLAUSE",
        [END_CLAUSE] = "END_CLAUSE",
        [BIND] = "BIND",
        [APPLY] = "APPLY",
        [TYPE] = "TYPE",
        [QUOTE] = "QUOTE",
        [IMMEDIATE] = "IMMEDIATE",
};

#if TOKEN_DEBUG
#define LOG_TOKEN(m, typ, str) \
        printf(m ": %s \"%s\"\n", ubik_token_names[typ], str)
#else
#define LOG_TOKEN(m, typ, str)
#endif

#define TOKEN_BUFFER_SIZE 1024

static inline no_ignore ubik_error
emit(
        struct ubik_token *t,
        ssize_t *iref,
        ubik_tokenize_cb cb,
        void *cbarg)
{
        ubik_error err;
        ssize_t i;

        i = *iref;
        if (i < 0)
                return OK;

        t->str[i + 1] = '\0';
        LOG_TOKEN("emit", t->type, t->str);

        err = cb(t, cbarg);
        if (err != OK)
                return err;

        *iref = -1;
        return OK;
}

static inline no_ignore ubik_error
emit_last(
        struct ubik_token *t,
        ssize_t *iref,
        char c,
        ubik_tokenize_cb cb,
        void *cbarg)
{
        ubik_error err;
        ssize_t i;

        i = *iref;
        if (i < 1)
                return OK;

        t->str[i] = '\0';
        err = emit(t, iref, cb, cbarg);
        if (err != OK)
                return err;

        memset(t->str, 0x00, TOKEN_BUFFER_SIZE);
        t->str[0] = c;
        *iref = 0;
        return OK;
}

#define EL do {                                 \
        err = emit_last(&t, &i, c, cb, cb_arg); \
        if (err != OK) return err; } while (0)

#define E(typ) do {                             \
        t.type = typ;                           \
        err = emit(&t, &i, cb, cb_arg);         \
        if (err != OK) return err; } while (0)

#define DROP do { i = -1; } while (0)

no_ignore ubik_error
ubik_tokenize(
        ubik_tokenize_cb cb,
        struct ubik_stream *source,
        void *cb_arg)
{
        char accum[TOKEN_BUFFER_SIZE] = {0};
        char c;
        size_t r;
        ssize_t i;
        struct ubik_token t;
        ubik_error err;

        i = -1;
        t.str = accum;

        while ((r = ubik_stream_read(&accum[++i], source, 1)) == 1)
        {
                c = accum[i];
                printf("'%c'\n", c);
                accum[i + 1] = '\0';
                switch (c)
                {
                case '[': EL; E(BLOCK_OPEN); break;
                case ']': EL; E(BLOCK_CLOSE); break;
                case '_': EL; E(START_CLAUSE); break;
                case '.': EL; E(END_CLAUSE); break;
                case ':': EL; E(BIND); break;
                case '<': EL; E(APPLY); break;
                case '^': EL; E(TYPE); break;
                case '@': EL; E(QUOTE); break;
                case '!': EL; E(IMMEDIATE); break;

                case ' ':
                case '\n':
                case '\r':
                case '\t':
                        accum[i--] = '\0';
                        EL;
                        break;

                default:
                        t.type = NAME;
                        accum[i + 1] = '\0';
                        break;
                }
        }

        return OK;
}

