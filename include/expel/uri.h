/*
 * uri.h: expel content identifiers
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

#include <stdbool.h>
#include <stdint.h>

#include "expel/expel.h"

/* Identifies values in the expel substrate */
struct xl_uri {
        xl_tag    tag;
        xl_word   hash;

        char      *name;
        size_t    name_len;
        xl_word   version;
        xl_word   scope;

        uint64_t  refcount;
};

/* Creates a URI for a local resource. */
no_ignore xl_error
xl_uri_user(
        struct xl_uri *uri,
        char *name);

/* Creates a URI for a local resource. */
no_ignore xl_error
xl_uri_native(
        struct xl_uri *uri,
        char *name);

/* Creates a URI struct from a value-encoded URI. */
no_ignore xl_error
xl_uri_from_value(struct xl_uri *uri, struct xl_value *uri_val);

/* Returns true if the provided URIs are equal. */
bool
xl_uri_eq(struct xl_uri *u0, struct xl_uri *u1);
