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

#include <stdint.h>

/* left is a function and right is its argument */
#define TAG_APPLY       0x00
/* left is a type object and right is a value with the given type */
#define TAG_TYPED       0x01
/* left is a type object and right is the name of the noun */
#define TAG_NOUN        0x02

/* value is a 64-bit unsigned integer, extra is NULL */
#define BASE_TYPE_U64   0x00
/* value is a 8-bit unsigned integer, extra is NULL */
#define BASE_TYPE_U8    0x01

typedef uint8_t tag_t;

union xl_ptr_val {
        struct xl_value *p;
        struct xl_type *t;
        uint64_t v;
};

struct xl_value {
        union xl_ptr_val left;
        union xl_ptr_val right;
        tag_t tag;
};

struct xl_derived_type {
        void *unused;
};

struct xl_type {
        uint64_t base;
        struct xl_derived_type *extra;
};

#define VAL_CTOR(val_name, val_type) \
        void make_##val_name(val_type val, struct xl_value *out); \
        val_type get_##val_name(struct xl_value *out)

VAL_CTOR(u8, uint8_t);
VAL_CTOR(u64, uint64_t);
VAL_CTOR(string, const char *);

void
eval(struct xl_value in, struct xl_value out);
