/*
 * env.h: ubik environment definitions
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

#include "ubik/ubik.h"
#include "ubik/uri.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>


typedef ubik_error (*ubik_env_cb)(
        void *arg,
        struct ubik_env *env,
        struct ubik_uri *uri);

/* An association between a URI and a value */
struct ubik_binding
{
        struct ubik_uri           *uri;
        union ubik_value_or_graph value;
        struct ubik_value         *type;
};

struct ubik_env_watch
{
        struct ubik_uri *uri;
        struct ubik_env *target_env;
        ubik_env_cb cb;
        void *arg;

        /* Fired is set to true once the watcher has fired once; if this is
         * true, it means means that the watcher is dead. */
        bool fired;

        /* The number of environments on which this watch was placed. */
        ubik_word refcount;
};

struct ubik_env_watch_list
{
        struct ubik_env_watch *watch;

        struct ubik_env_watch_list *prev;
        struct ubik_env_watch_list *next;
};

/* A hash mapping from URI to value. */
struct ubik_env
{
        struct ubik_binding *bindings;
        size_t n;
        size_t cap;
        struct ubik_env *parent;

        struct ubik_env_watch_list *watches;
};

/* Initializes a new environment struct. */
no_ignore ubik_error
ubik_env_init(struct ubik_env *env);

/* Creates a child environment from the given env. */
no_ignore ubik_error
ubik_env_make_child(struct ubik_env *child, struct ubik_env *parent);

/* Returns the root environment.
 *
 * This environment is the parent of any environment that isn't
 * explicitly created as a child. */
struct ubik_env *
ubik_env_get_root();

/* Frees memory associated with the environment struct.
 *
 * Returns OK if successful. If this method returns an error code,
 * there is no guarantee that the environment is in a usable
 * state. If OK is returned, the environment can still be used
 * with ubik_get and ubik_set after calling ubik_env_free; this is a
 * clear operation that does not destroy the env. */
no_ignore ubik_error
ubik_env_free(struct ubik_env *env);

/* Finds the value associated wth the given URI in the environment.
 *
 * If the value is found, OK is returned and the out pointer is
 * set to the pointer to the assigned value. If the value is not
 * found, ERR_ABSENT is returned and the out pointer is unchanged.
 * */
no_ignore ubik_error
ubik_env_get(
        union ubik_value_or_graph *value,
        struct ubik_value **type,
        struct ubik_env *env,
        struct ubik_uri *uri);

/* Inserts the given value in at the given URI, overwriting an
 * existing value if present.
 *
 * The URI is copied into the environment but the value is not;
 * later modifications to the passed-in URI will not change the
 * bindings but modifications to the value will modify the value
 * stored in the environment. */
no_ignore ubik_error
ubik_env_overwrite(
        struct ubik_env *env,
        struct ubik_uri *uri,
        union ubik_value_or_graph value,
        struct ubik_value *type);

/* Inserts the given value in at the given URI if the URI is
 * not already defined.
 *
 * If the URI is already bound, this will return ERR_PRESENT
 * and the existing binding will not be modified.
 *
 * The URI is copied into the environment but the value is not;
 * later modifications to the passed-in URI will not change the
 * bindings but modifications to the value will modify the value
 * stored in the environment. */
no_ignore ubik_error
ubik_env_set(
        struct ubik_env *env,
        struct ubik_uri *uri,
        union ubik_value_or_graph value,
        struct ubik_value *type);

/* Returns true if the provided URI is present in the environment. */
no_ignore ubik_error
ubik_env_present(
        bool *is_present,
        struct ubik_env *env,
        struct ubik_uri *uri);

/* Calls the provided callback function once for every item. */
no_ignore ubik_error
ubik_env_iterate(
        ubik_env_cb callback,
        struct ubik_env *env,
        void *callback_arg);

/* Calls the provided callback function when a URI is inserted or modified.
 *
 * Note that a watch is only ever triggered once; after being triggered it is
 * removed from the list of watchers. */
no_ignore ubik_error
ubik_env_watch(
        ubik_env_cb callback,
        struct ubik_env *env,
        struct ubik_uri *uri,
        void *callback_arg);

