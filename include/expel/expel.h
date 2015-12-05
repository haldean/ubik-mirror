/*
 * expel.h: expel base types
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

#ifndef EXPEL_EXPEL_H
#define EXPEL_EXPEL_H

#include <stddef.h>
#include <stdint.h>

#include "expel/const.h"
#include "expel/words.h"

typedef uint8_t tag_t;
typedef uint64_t word_t;

#define TAG_LEFT_NODE   0x01
#define TAG_LEFT_WORD   0x02
#define TAG_RIGHT_NODE  0x04
#define TAG_RIGHT_WORD  0x08

union _xl_ptr_val {
        struct xl_value *p;
        word_t v;
};
struct xl_value {
        union _xl_ptr_val left;
        union _xl_ptr_val right;
        tag_t tag;
};

// identifies a user in the expel substrate
struct xl_user {
        uint64_t id;
        char *name;
};

// identifies content in the expel substrate
struct xl_uri {
        char *name;
        uint64_t version;
        struct xl_user author;
        uint8_t scope;
};

struct xl_env;
struct xl_stream;

void
xl_load(struct xl_value *out, struct xl_stream *sp);

void
xl_save(struct xl_stream *sp, struct xl_value *in);

#endif
