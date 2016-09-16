/*
 * uri.h: ubik content identifiers
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

#pragma once
#include <stdbool.h>
#include <stdint.h>

#include "ubik/ubik.h"

/* Identifies values in the ubik substrate */
struct ubik_uri {
        ubik_word hash;

        char      *name;
        size_t    name_len;
        char      *source;
        size_t    source_len;
        ubik_word version;
        ubik_word scope;

        /* The value representation of the URI. Generated and cached by
         * ubik_uri_attach_value. */
        struct ubik_value *as_value;
};

/* Creates a URI for a resource of unknown scope. */
no_ignore ubik_error
ubik_uri_unknown(
        struct ubik_uri *uri,
        char *name);

/* Creates a URI for a local resource. */
no_ignore ubik_error
ubik_uri_user(
        struct ubik_uri *uri,
        char *name);

/* Creates a URI for a package-scoped resource. */
no_ignore ubik_error
ubik_uri_package(
        struct ubik_uri *uri,
        char *package,
        char *name);

/* Creates a URI for a package-scoped resource, but does not copy in the
 * name or package. Guaranteed to allocate no memory. */
no_ignore ubik_error
ubik_uri(
        struct ubik_uri *uri,
        char *package,
        char *name);

/* Creates a URI for a global, runtime-provided resource. */
no_ignore ubik_error
ubik_uri_native(
        struct ubik_uri *uri,
        char *name);

/* Creates a URI struct from a value-encoded URI. */
no_ignore ubik_error
ubik_uri_from_value(struct ubik_uri *uri, struct ubik_value *uri_val);

/* Returns true if the provided URIs are equal. */
bool
ubik_uri_eq(struct ubik_uri *u0, struct ubik_uri *u1);

/* Generates a value representation for the URI and stores it in the URI's
 * as_value field. If the field is already initialized, this function is a
 * no-op. */
no_ignore ubik_error
ubik_uri_attach_value(struct ubik_uri *uri, struct ubik_workspace *ws);

/* Parses a URI string into a URI struct. Returns struct has no references;
 * caller must take a reference to it. */
no_ignore ubik_error
ubik_uri_parse(struct ubik_uri *uri, char *str);

/* Creates a string representation of a URI. */
char *
ubik_uri_explain(struct ubik_uri *uri);
