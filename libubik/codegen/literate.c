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
#include "ubik/passthrough.h"
#include "ubik/stream.h"

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#define colon_match "::\n"
#define colon_match_at 3
#define code_match "..code::ubik\n"
#define code_match_at 13

struct literate_state
{
        struct ubik_generator head;
        struct ubik_stream *base;

        /* True if we're inside ubik source material, false otherwise. */
        bool in_block;
        /* The lengths that we've matched the code and colon pattern starts,
         * respectively. When these are equal to code_match_at/colon_match_at,
         * we've successfully matched a block start. */
        size_t code_match_len;
        size_t colon_match_len;
        /* True if the last character we saw was a newline. */
        bool just_saw_nl;
};

#define ls(gen) ((struct literate_state *)gen)

static bool
isws(char c)
{
        return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

/* Called to copy source material to a destination buffer until the end of the
 * source material is reached. Returns the amount of source material consumed. */
static size_t
copy_in_block(struct literate_state *ls, char *dst, char *src, size_t len)
{
        size_t i;
        char s;

        ls->just_saw_nl = false;
        for (i = 0; i < len; i++)
        {
                s = src[i];
                if (ls->just_saw_nl && !isws(s))
                {
                        ls->in_block = false;
                        ls->just_saw_nl = false;
                        break;
                }
                *(dst++) = s;
                ls->just_saw_nl = s == '\n';
        }

        return i;
}

/* Called to discard non-source-material. Returns the number of characters
 * discarded. */
static size_t
find_block_start(struct literate_state *ls, char *src, size_t len)
{
        size_t i;
        char s;

        for (i = 0; i < len; i++)
        {
                s = src[i];

                if (s == colon_match[ls->colon_match_len])
                        ls->colon_match_len++;
                else
                        ls->colon_match_len = 0;
                if (ls->colon_match_len == colon_match_at)
                {
                        ls->colon_match_len = 0;
                        ls->code_match_len = 0;
                        ls->in_block = true;
                        ls->just_saw_nl = s == '\n';
                        return i+1;
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
                        ls->in_block = true;
                        ls->just_saw_nl = s == '\n';
                        return i+1;
                }

                ls->just_saw_nl = s == '\n';
        }
        return i;
}

static size_t
read_lit(void *vdst, struct ubik_generator *gen, size_t len)
{
        size_t base_read;
        size_t dst_filled;
        size_t buf_used;
        size_t t;
        size_t iter;
        char *buf;
        char *dst;

        dst = (char *) vdst;
        buf = calloc(len, 1);
        if (buf == NULL)
                return 0;
        dst_filled = 0;

        /* This iteration variable is just here to make sure that a bug doesn't
         * cause this to loop forever. This process should never loop more than
         * a handful of times, but it is totally broken if it tries to run more
         * than the size of the destination buffer. */
        for (iter = 0; iter < len; iter++)
        {
                base_read = ubik_stream_read(buf, ls(gen)->base, len);
                buf_used = 0;
                while (buf_used < base_read)
                {
                        if (ls(gen)->in_block)
                        {
                                t = copy_in_block(
                                        ls(gen), dst + dst_filled,
                                        buf + buf_used, base_read - buf_used);
                                buf_used += t;
                                dst_filled += t;
                                /* If we're still in a block at the end of reading,
                                 * we're done here. */
                                if (ls(gen)->in_block)
                                        goto done;
                        }
                        /* Can't use "else" here, this can change as part of
                         * copy_in_block. */
                        if (!ls(gen)->in_block)
                        {
                                t = find_block_start(
                                        ls(gen), buf + buf_used,
                                        base_read - buf_used);
                                ubik_assert(t > 0);
                                buf_used += t;
                        }
                }
        }

done:
        free(buf);
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
        ubik_stream_reset(ls(gen)->base);
        ls(gen)->in_block = false;
        ls(gen)->code_match_len = false;
        ls(gen)->colon_match_len = false;
        ls(gen)->just_saw_nl = false;
}

static FILE *
fp_lit(struct ubik_generator *gen)
{
#define bufsize 4096
        char *contents;
        size_t size;
        char buf[bufsize];
        size_t read;
        FILE *f;

        f = open_memstream(&contents, &size);
        do
        {
                read = read_lit(buf, gen, bufsize);
                if (read > 0)
                        fwrite(buf, 1, read, f);
        } while (read == bufsize);
        fclose(f);

        f = fmemopen(contents, size, "r");
        return f;
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
        char *src_filename)
{
        struct literate_state *lit;

        if (!is_literate_ext(src_filename))
                return ubik_passthrough_new(res, src, false);

        lit = calloc(1, sizeof(struct literate_state));
        if (lit == NULL)
                return ubik_raise(ERR_NO_MEMORY, "literate state alloc");

        lit->head.read = read_lit;
        lit->head.write = NULL;
        lit->head.drop = NULL;
        lit->head.reset = reset_lit;
        lit->head.close = close_lit;
        lit->head.fp = fp_lit;
        lit->base = src;
        return ubik_stream_generator(res, &lit->head);
}

