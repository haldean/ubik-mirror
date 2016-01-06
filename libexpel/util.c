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

#include "expel/expel.h"
#include "expel/util.h"

#include <arpa/inet.h>
#include <stdint.h>
#include <stdlib.h>

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

// converts a word from host byte order to network byte order
word_t
htonw(word_t w)
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
        // byte order test macros aren't defined, we have to roll our own
        // runtime tests instead.
        uint32_t test_int = 1;
        if (*((char *) &test_int) == 0)
                return w;
        return ((uint64_t) htonl(w >> 32)) |
               ((uint64_t) htonl((uint32_t) w) << 32);
#endif
}

// converts a word from network byte order to host byte order
word_t
ntohw(word_t w)
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
        // byte order test macros aren't defined, we have to roll our own
        // runtime tests instead.
        uint32_t test_int = 1;
        if (*((char *) &test_int) == 0)
                return w;
        return ((uint64_t) ntohl(w >> 32)) |
               ((uint64_t) ntohl((uint32_t) w)) << 32;
#endif
}

char *
xl_explain_word(word_t word)
{
        size_t i;
        char *res;
        word = htonw(word);
        res = calloc(9, sizeof(char));
        if (res == NULL)
                return res;
        for (i = 0; i < 8; i++)
                res[i] = (char) (word >> (8 * (7 - i)));
        return res;
}
