/*
 * util.c: implementation of internal runtime utilities
 * Copyright (C) 2015, Haldean Brown
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
#include "ubik/util.h"

#include <arpa/inet.h>
#include <execinfo.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

size_t
size_max(size_t a, size_t b)
{
        return a < b ? b : a;
}

size_t
size_min(size_t a, size_t b)
{
        return a < b ? a : b;
}

ubik_word
htonw(ubik_word w)
{
#ifdef __BYTE_ORDER__
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
        return w;
#elif __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
        return ((uint64_t) htonl(w >> 32)) |
               ((uint64_t) htonl((uint32_t) w) << 32);
#else
#error "unsupported byte order" __BYTE_ORDER__
#endif
#else
        /* byte order test macros aren't defined, we have to roll our own
         * runtime tests instead. */
        uint32_t test_int = 1;
        if (*((char *) &test_int) == 0)
                return w;
        return ((uint64_t) htonl(w >> 32)) |
               ((uint64_t) htonl((uint32_t) w) << 32);
#endif
}

ubik_word
ntohw(ubik_word w)
{
#ifdef __BYTE_ORDER__
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
        return w;
#elif __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
        return ((uint64_t) ntohl(w >> 32)) |
               ((uint64_t) ntohl((uint32_t) w)) << 32;
#else
#error "unsupported byte order" __BYTE_ORDER__
#endif
#else
        /* byte order test macros aren't defined, we have to roll our own
         * runtime tests instead. */
        uint32_t test_int = 1;
        if (*((char *) &test_int) == 0)
                return w;
        return ((uint64_t) ntohl(w >> 32)) |
               ((uint64_t) ntohl((uint32_t) w)) << 32;
#endif
}

#define UBIK_TRACE_SIZE 128

/* Obtain a backtrace and print it to stdout. */
void
ubik_trace_print(void)
{
        void *array[UBIK_TRACE_SIZE];
        size_t size;

        size = backtrace(array, UBIK_TRACE_SIZE);
        backtrace_symbols_fd(array, size, STDERR_FILENO);
}

void
ubik_trace_get(char ***res, size_t *n_lines)
{
        void *array[UBIK_TRACE_SIZE];
        size_t size;

        size = backtrace(array, UBIK_TRACE_SIZE);
        *res = backtrace_symbols(array, size);
        *n_lines = size;
}

char *
ubik_word_explain(ubik_word word)
{
        size_t i;
        char *res;
        res = calloc(9, sizeof(char));
        if (res == NULL)
                return res;
        for (i = 0; i < 8; i++)
                res[i] = (char) (word >> (8 * (7 - i)));
        return res;
}
