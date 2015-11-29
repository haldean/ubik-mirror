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

#include <stddef.h>
#include <stdint.h>

#include "expel/words.h"

#define TAG_LEFT_NODE       0x01
#define TAG_LEFT_WORD       0x02
#define TAG_RIGHT_NODE      0x04
#define TAG_RIGHT_WORD      0x08

#define BASE_TYPE_WORD      pack("....word")
#define BASE_TYPE_SINT64    pack("..sint64")

typedef uint8_t tag_t;
typedef uint64_t word_t;

union xl_ptr_val {
        struct xl_value *p;
        word_t v;
};

struct xl_value {
        union xl_ptr_val left;
        union xl_ptr_val right;
        tag_t tag;
};

#define VAL_CTOR(val_name, val_type) \
        void make_##val_name(val_type val, struct xl_value *out); \
        val_type get_##val_name(struct xl_value *out)

VAL_CTOR(word, word_t);
VAL_CTOR(string, const char *);

void
eval(struct xl_value in, struct xl_value out);
