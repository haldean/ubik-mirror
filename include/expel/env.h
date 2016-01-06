/*
 * env.h: expel environment definitions
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

#ifndef EXPEL_ENV_H
#define EXPEL_ENV_H

#include "expel/expel.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>


/* Identifies values in the expel substrate */
struct xl_uri {
        uint64_t hash;
        char     *name;
        uint64_t __reserved_for_author;
        uint32_t version;
        uint16_t __padding_0;
        uint8_t  scope;
        uint8_t  __padding_1;
};

/* An association between a URI and a value */
struct xl_binding {
        struct xl_uri   *uri;
        struct xl_value *value;
        struct xl_value *type;
};

/* A hash mapping from URI to value. */
struct xl_env {
        struct xl_binding *bindings;
        size_t            n;
        size_t            cap;
};

/* Creates a URI for a local resource. */
no_ignore word_t
xl_uri_local(
        struct xl_uri *uri,
        char *name);

/* Returns true if the provided URIs are equal. */
bool
xl_uri_eq(struct xl_uri *u0, struct xl_uri *u1);

/* Initializes a new environment struct. */
no_ignore word_t
xl_env_init(struct xl_env *env);

/* Frees memory associated with the environment struct.
 *
 * Returns OK if successful. If this method returns an error code,
 * there is no guarantee that the environment is in a usable
 * state. If OK is returned, the environment can still be used
 * with xl_get and xl_set after calling xl_env_free; this is a
 * clear operation that does not destroy the env. */
no_ignore word_t
xl_env_free(struct xl_env *env);

/* Finds the value associated wth the given URI in the environment.
 *
 * If the value is found, OK is returned and the out pointer is
 * set to the pointer to the assigned value. If the value is not
 * found, ERR_ABSENT is returned and the out pointer is unchanged.
 * */
no_ignore word_t
xl_get(
        struct xl_value **value,
        struct xl_value **type,
        struct xl_env *env,
        struct xl_uri *uri);

/* Inserts the given value in at the given URI.
 *
 * The URI is copied into the environment but the value is not;
 * later modifications to the passed-in URI will not change the
 * bindings but modifications to the value will modify the value
 * stored in the environment. */
no_ignore word_t
xl_set(
        struct xl_env *env,
        struct xl_uri *uri,
        struct xl_value *value,
        struct xl_value *type);

#endif
