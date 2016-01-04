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

// identifies a user in the expel substrate
struct xl_user {
        uint64_t id;
        char *name;
};

// identifies content in the expel substrate
struct xl_uri {
        uint64_t hash;
        char     *name;
        uint64_t __reserved_for_author;
        uint32_t version;
        uint16_t __padding_0;
        uint8_t  scope;
        uint8_t  __padding_1;
};

struct xl_binding {
        struct xl_uri   *uri;
        struct xl_value *value;
};

struct xl_env {
        struct xl_binding *bindings;
        size_t            n;
        size_t            cap;
};

word_t
xl_uri_local(
        struct xl_uri *uri,
        char *name);

bool
xl_uri_eq(struct xl_uri *u0, struct xl_uri *u1);

word_t
xl_env_init(struct xl_env *env);

word_t
xl_get(struct xl_value **out, struct xl_env *env, struct xl_uri *uri);

word_t
xl_set(struct xl_env *env, struct xl_uri *uri, struct xl_value *value);

#endif
