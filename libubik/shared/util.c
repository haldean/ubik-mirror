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
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/syscall.h>
#include <unistd.h>

#ifdef __GLIBC__
#include <execinfo.h>
#endif

#ifndef __has_builtin
#define __has_builtin(x) 0
#endif

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

#ifdef __GLIBC__
/* Obtain a backtrace and print it to stdout. */
void
ubik_trace_print(void)
{
        void *array[UBIK_TRACE_SIZE];
        size_t size;

        size = backtrace(array, UBIK_TRACE_SIZE);
        backtrace_symbols_fd(array, size, STDOUT_FILENO);
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
#else
void
ubik_trace_print(void)
{
        printf("stack traces not supported on this platform.\n");
}

void
ubik_trace_get(char ***res, size_t *n_lines)
{
        *res = NULL;
        *n_lines = 0;
}
#endif

char *
ubik_word_explain(ubik_word word)
{
        size_t i, j;
        char *res;
        char w;
        res = calloc(9, sizeof(char));
        if (res == NULL)
                return res;
        for (i = 0, j = 0; i < 8; i++)
        {
                w = (char) (word >> (8 * (7 - i)));
                if (w != ' ' || j > 0)
                        res[j++] = w;
        }
        return res;
}

no_ignore ubik_error
ubik_check_add(ubik_word *res, ubik_word w1, ubik_word w2)
{
#if __GNUC__ >= 5 || __has_builtin(__builtin_uaddl_overflow)
        if (__builtin_uaddl_overflow(w1, w2, res))
                return ubik_raise(ERR_OVERFLOW, "addition overflowed");
        return OK;
#else
        ubik_word r;
        r = w1 + w2;
        if (r < w1 || r < w2)
                return ubik_raise(ERR_OVERFLOW, "addition overflowed");
        *res = r;
        return OK;
#endif
}

int64_t
ubik_gettid()
{
        return (int64_t) syscall(SYS_gettid);
}
