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

#pragma once

#include "ubik/ubik.h"
#include "ubik/uri.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>


typedef xl_error (*xl_env_cb)(
        void *arg,
        struct xl_env *env,
        struct xl_uri *uri);

/* An association between a URI and a value */
struct xl_binding
{
        struct xl_uri           *uri;
        union xl_value_or_graph value;
        struct xl_value         *type;
};

struct xl_env_watch
{
        struct xl_uri *uri;
        struct xl_env *target_env;
        xl_env_cb cb;
        void *arg;

        /* Fired is set to true once the watcher has fired once; if this is
         * true, it means means that the watcher is dead. */
        bool fired;

        /* The number of environments on which this watch was placed. */
        xl_word refcount;
};

struct xl_env_watch_list
{
        struct xl_env_watch *watch;

        struct xl_env_watch_list *prev;
        struct xl_env_watch_list *next;
};

/* A hash mapping from URI to value. */
struct xl_env
{
        struct xl_binding *bindings;
        size_t n;
        size_t cap;
        struct xl_env *parent;

        struct xl_env_watch_list *watches;
};

/* Initializes a new environment struct. */
no_ignore xl_error
xl_env_init(struct xl_env *env);

/* Creates a child environment from the given env. */
no_ignore xl_error
xl_env_make_child(struct xl_env *child, struct xl_env *parent);

/* Returns the root environment.
 *
 * This environment is the parent of any environment that isn't
 * explicitly created as a child. */
struct xl_env *
xl_env_get_root();

/* Frees memory associated with the environment struct.
 *
 * Returns OK if successful. If this method returns an error code,
 * there is no guarantee that the environment is in a usable
 * state. If OK is returned, the environment can still be used
 * with xl_get and xl_set after calling xl_env_free; this is a
 * clear operation that does not destroy the env. */
no_ignore xl_error
xl_env_free(struct xl_env *env);

/* Finds the value associated wth the given URI in the environment.
 *
 * If the value is found, OK is returned and the out pointer is
 * set to the pointer to the assigned value. If the value is not
 * found, ERR_ABSENT is returned and the out pointer is unchanged.
 * */
no_ignore xl_error
xl_env_get(
        union xl_value_or_graph *value,
        struct xl_value **type,
        struct xl_env *env,
        struct xl_uri *uri);

/* Inserts the given value in at the given URI, overwriting an
 * existing value if present.
 *
 * The URI is copied into the environment but the value is not;
 * later modifications to the passed-in URI will not change the
 * bindings but modifications to the value will modify the value
 * stored in the environment. */
no_ignore xl_error
xl_env_overwrite(
        struct xl_env *env,
        struct xl_uri *uri,
        union xl_value_or_graph value,
        struct xl_value *type);

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
no_ignore xl_error
xl_env_set(
        struct xl_env *env,
        struct xl_uri *uri,
        union xl_value_or_graph value,
        struct xl_value *type);

/* Returns true if the provided URI is present in the environment. */
no_ignore xl_error
xl_env_present(
        bool *is_present,
        struct xl_env *env,
        struct xl_uri *uri);

/* Calls the provided callback function once for every item. */
no_ignore xl_error
xl_env_iterate(
        xl_env_cb callback,
        struct xl_env *env,
        void *callback_arg);

/* Calls the provided callback function when a URI is inserted or modified.
 *
 * Note that a watch is only ever triggered once; after being triggered it is
 * removed from the list of watchers. */
no_ignore xl_error
xl_env_watch(
        xl_env_cb callback,
        struct xl_env *env,
        struct xl_uri *uri,
        void *callback_arg);

