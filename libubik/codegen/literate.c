/*
 * literate.c: work with literate Ubik files.
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

#include "ubik/assert.h"
#include "ubik/literate.h"
#include "ubik/stream.h"

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#define colon_match "::\n"
#define colon_match_at 3
#define code_match "..code::ubik\n"
#define code_match_at 13

#define ls_buf_size 1024

struct literate_state
{
        struct ubik_generator head;
        struct ubik_stream *base;

        /* True if the stream is unliterate and doesn't need weaving, false if
         * it needs to be woven. */
        bool raw;

        /* True if we're inside ubik source material, false otherwise. */
        bool in_source;

        /* The lengths that we've matched the code and colon pattern starts,
         * respectively. When these are equal to code_match_at/colon_match_at,
         * we've successfully matched a block start. */
        size_t code_match_len;
        size_t colon_match_len;

        /* True if the last character we saw was a newline. */
        bool just_saw_nl;

        /* If not '\0', then this is a character that needs to be inserted
         * before any other characters are. */
        char to_insert;

        /* The buffer that input bytes are read into, so that buffer state can
         * persist across reads. */
        char buf[ls_buf_size];
        size_t buf_loc;
        size_t buf_end;
};

#define asls(gen) ((struct literate_state *)gen)

static bool
isws(char c)
{
        return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

/* Called to update in_source in the literate state struct. */
static void
feed_source_match(struct literate_state *ls, char s)
{
        if (s == colon_match[ls->colon_match_len])
                ls->colon_match_len++;
        else
                ls->colon_match_len = 0;
        if (ls->colon_match_len == colon_match_at)
        {
                ls->colon_match_len = 0;
                ls->code_match_len = 0;
                ls->in_source = true;
                ls->just_saw_nl = s == '\n';
                return;
        }

        if (s == code_match[ls->code_match_len])
        {
                /* The code match must start at the start of a line. */
                if (ls->code_match_len != 0 || ls->just_saw_nl)
                        ls->code_match_len++;
        }
        /* The code match is whitespace-tolerant, as long as there's no
         * newlines inside it. */
        else if (s == '\n' || !isws(s))
                ls->code_match_len = 0;

        if (ls->code_match_len == code_match_at)
        {
                ls->colon_match_len = 0;
                ls->code_match_len = 0;
                ls->in_source = true;
                ls->just_saw_nl = s == '\n';
                return;
        }

        ls->just_saw_nl = s == '\n';
}

/* Refill the literate-state buffer, returning the number of bytes remaining in
 * the buffer. */
static size_t
refill_buf(struct literate_state *ls)
{
        ubik_assert(ls->buf_loc <= ls->buf_end);

        if (ls->buf_loc < ls->buf_end && ls->buf_loc > 0)
        {
                /* If there's still some stuff in the buffer, and it's not
                 * already at the start, copy that to the front of the buffer
                 * and start filling from there. */
                memmove(ls->buf, &ls->buf[ls->buf_loc],
                        ls->buf_end - ls->buf_loc);
                ls->buf_end -= ls->buf_loc;
                ls->buf_loc = 0;
        }
        else
        {
                /* Otherwise, reset the buffer's state and start writing from
                 * the start. */
                ls->buf_end = 0;
                ls->buf_loc = 0;
        }

        /* Now read into the buffer, filling as much of the remaining space as
         * possible. */
        ls->buf_end += ubik_stream_read(
                &ls->buf[ls->buf_end], ls->base, ls_buf_size - ls->buf_end);
        return ls->buf_end;
}

static size_t
read_lit(void *vdst, struct ubik_generator *gen, size_t len)
{
        struct literate_state *ls;
        size_t dst_filled;
        size_t to_copy;
        char *dst;
        char s;

        dst = (char *) vdst;
        ls = asls(gen);
        dst_filled = 0;

        if (ls->raw)
        {
                while (dst_filled < len)
                {
                        to_copy = size_min(
                                len - dst_filled,
                                ls->buf_end - ls->buf_loc);
                        memcpy(dst, &ls->buf[ls->buf_loc], to_copy);

                        ls->buf_loc += to_copy;
                        dst_filled += to_copy;

                        if (refill_buf(ls) == 0)
                                break;
                }
                return dst_filled;
        }

        while (dst_filled < len)
        {
                if (ls->to_insert != '\0')
                        dst[dst_filled++] = ls->to_insert;
                if (refill_buf(ls) == 0)
                        break;
                for (; ls->buf_loc < ls->buf_end && dst_filled < len; ls->buf_loc++)
                {
                        s = ls->buf[ls->buf_loc];

                        /* If we just saw a newline and we were in source
                         * before, we need to make sure we're still in source.
                         * We're only still in source material if this line
                         * starts with whitespace. */
                        if (ls->just_saw_nl && ls->in_source)
                                if (!isws(s))
                                        ls->in_source = false;

                        /* If we just saw a newline and we're not in source, we
                         * need to insert a comment marker at the start of the
                         * line. */
                        if (ls->just_saw_nl && !ls->in_source)
                        {
                                dst[dst_filled++] = '#';
                                if (dst_filled == len)
                                        ls->to_insert = ' ';
                                else
                                        dst[dst_filled++] = ' ';
                        }
                        if (dst_filled == len)
                                break;

                        feed_source_match(ls, s);
                        dst[dst_filled++] = s;
                }
        }
        return dst_filled;
}

static void
close_lit(struct ubik_generator *gen)
{
        free(gen);
}

static void
reset_lit(struct ubik_generator *gen)
{
        ubik_stream_reset(asls(gen)->base);
        asls(gen)->in_source = false;
        asls(gen)->code_match_len = false;
        asls(gen)->colon_match_len = false;
        asls(gen)->just_saw_nl = true;
}

static bool
is_literate_ext(char *src_filename)
{
        size_t flen;

        flen = strlen(src_filename);
        if (strcmp(src_filename + (flen - 3), ".ul") == 0)
                return true;
        if (strcmp(src_filename + (flen - 4), ".rst") == 0)
                return true;
        return false;
}

no_ignore ubik_error
ubik_literate_weave(
        struct ubik_stream *res,
        struct ubik_stream *src,
        char *src_filename,
        struct ubik_alloc_region *r)
{
        struct literate_state *ls;

        ubik_alloc1(&ls, struct literate_state, r);
        ls->head.read = read_lit;
        ls->head.write = NULL;
        ls->head.drop = NULL;
        ls->head.reset = reset_lit;
        ls->head.close = close_lit;
        ls->base = src;
        ls->raw = !is_literate_ext(src_filename);
        reset_lit(&ls->head);

        return ubik_stream_generator(res, &ls->head);
}

