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

typedef uint8_t tag_t;
typedef uint64_t word_t;

const tag_t  TAG_LEFT_NODE       = 0x01;
const tag_t  TAG_LEFT_WORD       = 0x02;
const tag_t  TAG_RIGHT_NODE      = 0x04;
const tag_t  TAG_RIGHT_WORD      = 0x08;

const word_t BASE_TYPE_WORD      = pack(' ', ' ', ' ', ' ', 'w', 'o', 'r', 'd');
const word_t BASE_TYPE_SINT64    = pack(' ', ' ', 's', 'i', 'n', 't', '6', '4');
const word_t BASE_TYPE_UINT32    = pack(' ', ' ', 'u', 'i', 'n', 't', '3', '2');
const word_t BASE_TYPE_SINT32    = pack(' ', ' ', 's', 'i', 'n', 't', '3', '2');
const word_t BASE_TYPE_UINT16    = pack(' ', ' ', 'u', 'i', 'n', 't', '1', '6');
const word_t BASE_TYPE_SINT16    = pack(' ', ' ', 's', 'i', 'n', 't', '1', '6');
const word_t BASE_TYPE_UINT08    = pack(' ', ' ', 'u', 'i', 'n', 't', '0', '8');
const word_t BASE_TYPE_SINT08    = pack(' ', ' ', 's', 'i', 'n', 't', '0', '8');
const word_t BASE_TYPE_LIST      = pack(' ', ' ', ' ', ' ', 'l', 'i', 's', 't');
const word_t BASE_TYPE_TUPLE     = pack(' ', ' ', ' ', 't', 'u', 'p', 'l', 'e');
const word_t BASE_TYPE_PACKED    = pack(' ', ' ', 'p', 'a', 'c', 'k', 'e', 'd');
const word_t BASE_TYPE_TYPE      = pack(' ', ' ', ' ', ' ', 't', 'y', 'p', 'e');

const word_t SEED_LAMBDA         = pack(' ', ' ', 'l', 'a', 'm', 'b', 'd', 'a');
const word_t SEED_APPLY          = pack(' ', ' ', ' ', 'a', 'p', 'p', 'l', 'y');
const word_t SEED_GET            = pack(' ', ' ', ' ', ' ', ' ', 'g', 'e', 't');
const word_t SEED_BIND           = pack(' ', ' ', ' ', ' ', 'b', 'i', 'n', 'd');

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

void
xl_load(struct xl_value *out, FILE *fp);

void
xl_save(FILE *fp, struct xl_value *in);
