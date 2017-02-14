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

#include <inttypes.h>
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
        [DEFINES] = "DEFINES",
        [IMPORT] = "IMPORT",
        [IMPORT_ALL] = "IMPORT_ALL",
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
        char c;
};

static const struct nfa_edge
edges[] = {
        { 0,  1,  '[' },
        { 0,  2,  ']' },
        { 0,  3,  '`' },
        { 3,  4,  '*' },
};

static const enum ubik_token_type
state_emits[] = {
        [0] = NONE,
        [1] = BLOCK_OPEN,
        [2] = BLOCK_CLOSE,
        [3] = IMPORT,
        [4] = IMPORT_ALL,
};

#define N_STATES (sizeof(state_emits) / sizeof(state_emits[0]))
#define N_EDGES (sizeof(edges) / sizeof(edges[0]))

static inline void
print_states(uint8_t states[N_STATES])
{
        uint_fast32_t s;
        for (s = 0; s < N_STATES; s++)
        {
                printf("%03" PRIuFAST32 "%c ", s, states[s] ? '#' : '_');
        }
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
                if (c != edge->c)
                        continue;
                if (!states[edge->s1])
                        continue;
                if (next_states[edge->s2])
                        continue;
                states_set++;
                next_states[edge->s2] = 1;
                if (state_emits[edge->s2] != NONE && terminal == 0)
                        terminal = edge->s2;
        }

        printf("'%c': ", c);
        print_states(states);
        printf("-> ");
        print_states(next_states);
        printf("[%" PRIuFAST32 "]\n", states_set);

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
                printf("accum: ");
                return res;
        }

        read = ubik_stream_read(&res, source, 1);
        if (read != 1)
                return -1;
        printf("strea: ");
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
        struct ubik_token t;
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
                        i -= last_terminal_i + 2;

                        LOG_TOKEN(t);
                        err = cb(&t, cb_arg);
                        if (err != OK)
                                return err;

                        last_terminal = 0;
                }
                else if (!states_set)
                {
                        /* Discard all input up until here, if we consumed a
                         * bunch and never saw a terminal. */
                        printf("drop: \"%s\"\n", t.str);
                        memset(t.str, 0x00, TOKEN_BUFFER_SIZE);
                        i = -1;
                }

                if (!states_set)
                        reset(states);
                else
                        memcpy(states, next_states, N_STATES);
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

