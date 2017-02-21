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
#include "ubik/assert.h"

#include <inttypes.h>
#include <string.h>

#define TOKEN_DEBUG 0

char *ubik_token_names[] = {
        [NONE] = "NONE",
        [BLOCK_OPEN] = "BLOCK_OPEN",
        [BLOCK_CLOSE] = "BLOCK_CLOSE",
        [NAME] = "NAME",
        [NUMBER] = "NUMBER",
        [BIND] = "BIND",
        [APPLY] = "APPLY",
        [TYPE] = "TYPE",
        [QUOTE] = "QUOTE",
        [IMMEDIATE] = "IMMEDIATE",
        [DEFINES] = "DEFINES",
        [IMPORT] = "IMPORT",
        [IMPORT_ALL] = "IMPORT_ALL",
        [STRING] = "STRING",
};

#if TOKEN_DEBUG
#define LOG_TOKEN(t) \
        printf("emit: %s \"%s\"\n", ubik_token_names[t.type], t.str)
#else
#define LOG_TOKEN(t)
#endif

#define TOKEN_BUFFER_SIZE 1024

typedef uint16_t state;

struct nfa_edge
{
        state s1;
        state s2;
        char c_lo;
        char c_hi;
};

static const struct nfa_edge
edges[] = {
        {  0,  1,   '[', '[' },
        {  0,  2,   ']', ']' },
        {  0,  3,   '`', '`' },
        {  3,  4,   '*', '*' },
        {  0,  5,   '0', '9' },
        {  5,  5,   '0', '9' },
        {  5,  5,   '.', '.' },
        {  0,  6,   '-', '-' },
        {  6,  5,   '0', '9' },
        {  0,  7,   'a', 'z' },
        {  0,  7,   'A', 'Z' },
        {  0,  7,   '_', '_' },
        {  7,  7,   'a', 'z' },
        {  7,  7,   'A', 'Z' },
        {  7,  7,   '0', '9' },
        {  7,  7,   '_', '_' },
        {  7,  7,   '-', '-' },
        {  7,  7,   ':', ':' },
        {  7,  7,  '\'', '\''},
        {  0,  8,   ':', ':' },
        {  0,  9,   '<', '<' },
        {  0, 10,   '^', '^' },
        {  0, 11,   '@', '@' },
        {  0, 12,   '!', '!' },
        {  0, 13,   '~', '~' },
        {  0, 14,   '"', '"' },
        /* These two match everything other than '"' */
        { 14, 14,     0, '!' },
        { 14, 14,   '#', 127 },
        { 14, 15,  '\\', '\\'},
        /* TODO: should only return to 14 on valid escapes */
        { 15, 14,     0, 127 },
        { 14, 16,   '"', '"' },
};

static const enum ubik_token_type
state_emits[] = {
        [ 0] = NONE,
        [ 1] = BLOCK_OPEN,
        [ 2] = BLOCK_CLOSE,
        [ 3] = IMPORT,
        [ 4] = IMPORT_ALL,
        [ 5] = NUMBER,
        /* have seen a '-', waiting for the rest of a number, maybe. */
        [ 6] = NONE,
        [ 7] = NAME,
        [ 8] = BIND,
        [ 9] = APPLY,
        [10] = TYPE,
        [11] = QUOTE,
        [12] = IMMEDIATE,
        [13] = DEFINES,
        /* have seen a ", waiting for the rest of the string */
        [14] = NONE,
        /* have seen a forward slash, next char is escaped */
        [15] = NONE,
        [16] = STRING,
};

#define N_STATES (sizeof(state_emits) / sizeof(state_emits[0]))
#define N_EDGES (sizeof(edges) / sizeof(edges[0]))

static inline void
print_states(uint8_t states[N_STATES])
{
        uint_fast32_t s;
        printf("{ ");
        for (s = 0; s < N_STATES; s++)
        {
                if (states[s])
                        printf("%03" PRIuFAST32 " ", s);
        }
        printf("} ");
}

static inline void
push_char(
        uint_fast32_t *states_set_ref,
        uint_fast32_t *terminal_ref,
        uint8_t next_states[N_STATES],
        uint8_t states[N_STATES],
        char c)
{
        const struct nfa_edge *edge;
        size_t e;
        uint_fast32_t states_set;
        uint_fast32_t terminal;

        memset(next_states, 0x00, N_STATES);
        states_set = 0;
        terminal = 0;
        for (e = 0; e < N_EDGES; e++)
        {
                edge = &edges[e];
                if (!states[edge->s1])
                        continue;
                if (c < edge->c_lo || edge->c_hi < c)
                        continue;
                if (next_states[edge->s2])
                        continue;
                states_set++;
                next_states[edge->s2] = 1;
                if (state_emits[edge->s2] != NONE && terminal == 0)
                        terminal = edge->s2;
        }

#if TOKEN_DEBUG
        if (c == '\n')
                printf("\\n : ");
        else if (c == '\r')
                printf("\\r : ");
        else
                printf("'%c': ", c);
        print_states(states);
        printf("-> ");
        print_states(next_states);
        printf("[%" PRIuFAST32 "]\n", states_set);
#endif

        *states_set_ref = states_set;
        *terminal_ref = terminal;
}

static inline int
next_char(
        char accum[TOKEN_BUFFER_SIZE],
        struct ubik_stream *source)
{
        size_t len;
        size_t read;
        char res;

        len = strlen(accum);
        if (len)
        {
                res = accum[len - 1];
                accum[len - 1] = '\0';
#if TOKEN_DEBUG
                printf("accum: ");
#endif
                return res;
        }

        read = ubik_stream_read(&res, source, 1);
        if (read != 1)
                return -1;
#if TOKEN_DEBUG
        printf("strea: ");
#endif
        return res;
}

static inline void
backtrack(
        char accum[TOKEN_BUFFER_SIZE],
        struct ubik_token *token,
        size_t last_terminal_i,
        size_t i)
{
        size_t j;
        for (j = strlen(accum); i > last_terminal_i; j++, i--)
                accum[j] = token->str[i];
        memset(&token->str[last_terminal_i + 1], 0x00,
               TOKEN_BUFFER_SIZE - last_terminal_i - 1);
}

static inline void
reset(uint8_t states[N_STATES])
{
        memset(states, 0x00, N_STATES);
        states[0] = 1;
}

static inline void
find_offsets(struct ubik_token *t)
{
        uint_fast32_t newlines;
        size_t i;
        size_t len;
        size_t last_nl;

        newlines = 0;
        len = strlen(t->str);
        last_nl = 0;
        for (i = 0; i < len; i++)
        {
                if (t->str[i] == '\n')
                {
                        newlines++;
                        last_nl = i;
                }
        }
        t->loc.line_end = t->loc.line_start + newlines;
        if (newlines)
                t->loc.col_end = len - last_nl - 1;
        else
                t->loc.col_end = t->loc.col_start + len;
}

no_ignore ubik_error
ubik_tokenize(
        ubik_tokenize_cb cb,
        struct ubik_stream *source,
        void *cb_arg)
{
        char accum[TOKEN_BUFFER_SIZE] = {0};
        char tokstr[TOKEN_BUFFER_SIZE] = {0};
        ssize_t i;
        ssize_t last_terminal_i;
        struct ubik_token t = {0};
        uint8_t states[N_STATES] = {0};
        uint8_t next_states[N_STATES] = {0};
        uint_fast32_t states_set;
        uint_fast32_t terminal;
        uint_fast32_t last_terminal;
        ubik_error err;
        char c;

        i = -1;
        t.str = tokstr;
        states[0] = 1;
        last_terminal = 0;

        while ((t.str[++i] = next_char(accum, source)) >= 0)
        {
                c = t.str[i];

                push_char(&states_set, &terminal, next_states, states, c);

                if (terminal)
                {
                        last_terminal_i = i;
                        last_terminal = terminal;
                }

                if (last_terminal && !states_set)
                {
                        t.type = state_emits[last_terminal];
                        backtrack(accum, &t, last_terminal_i, i);
                        find_offsets(&t);

                        LOG_TOKEN(t);
                        err = cb(&t, cb_arg);
                        if (err != OK)
                                return err;

                        last_terminal = 0;
                }
                else if (!states_set)
                {
                        /* We still want to find the offsets for the "token"
                         * we're throwing away. We only want to keep track of
                         * chars for location tracking that we're either
                         * committing or discarding; we can't just keep track
                         * as we read them, because the interaction with
                         * rewinding is hard. */
                        find_offsets(&t);
                }

                if (!states_set)
                {
                        t.loc.line_start = t.loc.line_end;
                        t.loc.col_start = t.loc.col_end;
                        memset(t.str, 0x00, TOKEN_BUFFER_SIZE);
                        reset(states);
                        i = -1;
                }
                else
                        memcpy(states, next_states, N_STATES);

                if (i == TOKEN_BUFFER_SIZE - 1)
                        return ubik_raise(
                                ERR_OVERFLOW, "exceeded token size limit");
        }

        if (last_terminal)
        {
                t.type = state_emits[last_terminal];
                backtrack(accum, &t, last_terminal_i, i);
                LOG_TOKEN(t);
                err = cb(&t, cb_arg);
                if (err != OK)
                        return err;
        }

        return OK;
}

