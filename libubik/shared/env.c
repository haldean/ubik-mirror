/*
 * env.c: ubik environment definitions
 * Copyright (C) 2016, Haldean Brown
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

#include <math.h>
#include <string.h>

#include "ubik/assert.h"
#include "ubik/ubik.h"
#include "ubik/env.h"
#include "ubik/util.h"

#define ENV_MAX_LOAD 0.5
#define ENV_INIT_CAP 8
#define ENV_CAP_SCALE 2

struct ubik_env *
ubik_env_get_root()
{
        static bool root_initialized = false;
        static struct ubik_env root;

        if (!root_initialized)
        {
                root_initialized = true;
                ubik_rwlock_init(&root.lock);
        }
        return &root;
}

no_ignore ubik_error
ubik_env_init(struct ubik_env *env)
{
        env->bindings = NULL;
        env->n = 0;
        env->cap = 0;
        env->parent = ubik_env_get_root();
        env->watches = NULL;
        env->initialized = true;
        ubik_rwlock_init(&env->lock);
        return OK;
}

no_ignore ubik_error
ubik_env_make_child(struct ubik_env *child, struct ubik_env *parent)
{
        ubik_error err;
        err = ubik_env_init(child);
        if (err != OK)
                return err;
        child->parent = parent;
        return OK;
}

no_ignore ubik_error
ubik_env_free(struct ubik_env *env)
{
        if (!env->initialized)
                return OK;
        ubik_rwlock_write(&env->lock);

        struct ubik_env_watch_list *to_free;
        size_t i;

        for (i = 0; i < env->cap; i++)
                if (env->bindings[i].uri != NULL)
                        ubik_uri_free(env->bindings[i].uri);
        if (likely(env->bindings != NULL))
                free(env->bindings);

        while (env->watches != NULL)
        {
                to_free = env->watches;
                env->watches = to_free->prev;

                if (to_free->watch->refcount > 1)
                        to_free->watch->refcount--;
                else
                        free(to_free->watch);
                free(to_free);
        }

        env->n = 0;
        env->cap = 0;
        env->bindings = NULL;

        env->watches = NULL;
        if (env != ubik_env_get_root())
                env->parent = ubik_env_get_root();

        ubik_rwlock_release(&env->lock);
        ubik_rwlock_destroy(&env->lock);
        return OK;
}

no_ignore ubik_error
ubik_env_iterate(
        ubik_env_cb callback,
        struct ubik_env *env,
        void *callback_arg)
{
        ubik_error err;
        size_t i;

        ubik_rwlock_read_scope(&env->lock);

        for (i = 0; i < env->cap; i++)
        {
                if (env->bindings[i].uri == NULL)
                        continue;
                err = callback(callback_arg, env, env->bindings[i].uri);
                if (err != OK)
                        return err;
        }

        return OK;
}

no_ignore ubik_error
ubik_env_get(
        struct ubik_value **value,
        struct ubik_value **type,
        struct ubik_env *env,
        struct ubik_uri *uri)
{
        size_t i;
        size_t probed;
        size_t h;
        bool found;

        ubik_rwlock_read_scope(&env->lock);

        found = false;

        if (env->cap != 0)
        {
                h = uri->hash % env->cap;
                for (probed = 0; probed < env->cap; probed++)
                {
                        i = (h + probed) % env->cap;
                        if (env->bindings[i].uri == NULL)
                                break;
                        if (env->bindings[i].uri->hash != uri->hash)
                                continue;
                        if (ubik_uri_eq(uri, env->bindings[i].uri))
                        {
                                if (value != NULL)
                                {
                                        *value = env->bindings[i].value;
                                        *type = env->bindings[i].type;
                                }
                                found = true;
                                break;
                        }
                }
        }

        if (found)
                return OK;

        ubik_assert(env->parent != env);
        if (env->parent == NULL)
                return ubik_raise(ERR_ABSENT, "ubik_get");
        return ubik_env_get(value, type, env->parent, uri);
}

no_ignore ubik_error
ubik_env_present(
        bool *is_present,
        struct ubik_env *env,
        struct ubik_uri *uri)
{
        ubik_error err;

        ubik_rwlock_read_scope(&env->lock);

        err = ubik_env_get(NULL, NULL, env, uri);
        if (err == OK)
                *is_present = true;
        else if (err->error_code == ERR_ABSENT)
        {
                free(err);
                *is_present = false;
        }
        else
                return err;
        return OK;
}

/* Inserts the given URI-value pair into the given binding array.
 *
 * This assumes that the table has space for another entry, and
 * will return ERR_FULL if the table has no spaces remaining.
 *
 * The overwrite parameter controls whether existing data will be
 * overwritten. True means that it will be, false means that it
 * will not. */
no_ignore static ubik_error
_insert(
        struct ubik_binding *binds,
        size_t cap,
        struct ubik_binding *insert,
        bool overwrite)
{
        size_t i;
        size_t probed;

        i = insert->uri->hash % cap;
        for (probed = 0; probed < cap; probed++)
        {
                if (binds[i].uri == NULL)
                        break;
                if (ubik_uri_eq(binds[i].uri, insert->uri))
                        break;
                i = (i + 1) % cap;
        }
        if (unlikely(probed == cap))
                return ubik_raise(ERR_FULL, "env insert");
        if (unlikely(binds[i].value != NULL))
        {
                if (likely(!overwrite))
                        return ubik_raise(ERR_PRESENT, "env overwrite");
                ubik_uri_free(binds[i].uri);
        }
        binds[i] = *insert;
        return OK;
}

/* Resizes and rebalances an environment, scaling the capacity by
 * ENV_CAP_SCALE as defined above.
 *
 * This first allocates a new bindings array, and then reinserts the
 * bindings from the old bindings array into the new one. Finally, it
 * frees the old array and updates the env struct to reference the new
 * array. If an error occurs during rebalancing, the environment remains
 * unmodified and a nonzero error code is returned. */
static ubik_error
_resize_rebalance(struct ubik_env *env)
{
        struct ubik_binding *new_binds;
        size_t new_cap;
        size_t i;
        size_t reinserted;
        ubik_error err;

        if (env->cap == 0)
                new_cap = ENV_INIT_CAP;
        else
                new_cap = ENV_CAP_SCALE * env->cap;

        new_binds = calloc(new_cap, sizeof(struct ubik_binding));
        if (new_binds == NULL)
                return ubik_raise(ERR_NO_MEMORY, "env resize");

        err = OK;
        for (i = 0, reinserted = 0; i < env->cap && reinserted < env->n; i++)
        {
                if (env->bindings[i].uri == NULL)
                        continue;

                err = _insert(new_binds, new_cap, &env->bindings[i], false);
                if (err != OK)
                        break;
        }

        /* if an error occurs, the environment remains unchanged. */
        if (err != OK)
        {
                free(new_binds);
                return err;
        }

        free(env->bindings);
        env->bindings = new_binds;
        env->cap = new_cap;
        return OK;
}

no_ignore static ubik_error
_set(
        struct ubik_env *env,
        struct ubik_uri *uri,
        struct ubik_value *value,
        struct ubik_value *type,
        bool overwrite)
{
        struct ubik_binding new_binding;
        struct ubik_env_watch_list *watch, *to_free;
        ubik_error err;

        err = OK;
        if (unlikely(env->cap == 0))
                err = _resize_rebalance(env);
        else if (unlikely((float) env->n / (float) env->cap > ENV_MAX_LOAD))
                err = _resize_rebalance(env);
        if (err != OK)
                return err;

        new_binding.uri = ubik_uri_dup(uri);
        new_binding.value = value;
        new_binding.type = type;

        err = _insert(env->bindings, env->cap, &new_binding, overwrite);
        if (err != OK)
        {
                ubik_uri_free(new_binding.uri);
                return err;
        }
        env->n++;

        watch = env->watches;
        while (watch != NULL)
        {
                if (ubik_uri_eq(watch->watch->uri, uri))
                {
                        err = watch->watch->cb(
                                watch->watch->arg,
                                watch->watch->target_env,
                                watch->watch->uri);
                        if (err != OK)
                                return err;

                        watch->watch->fired = true;
                }

                to_free = NULL;
                if (watch->watch->fired)
                {
                        watch->watch->refcount--;

                        to_free = watch;
                        if (watch->prev != NULL)
                                watch->prev->next = watch->next;
                        if (watch->next != NULL)
                                watch->next->prev = watch->prev;
                        if (env->watches == watch)
                                env->watches = watch->prev;
                }

                watch = watch->prev;

                if (to_free != NULL && to_free->watch->refcount == 0)
                        free(to_free->watch);
                if (to_free != NULL)
                        free(to_free);
        }

        return OK;
}

no_ignore ubik_error
ubik_env_set(
        struct ubik_env *env,
        struct ubik_uri *uri,
        struct ubik_value *value,
        struct ubik_value *type)
{
        ubik_rwlock_write_scope(&env->lock);
        return _set(env, uri, value, type, false);
}

no_ignore ubik_error
ubik_env_overwrite(
        struct ubik_env *env,
        struct ubik_uri *uri,
        struct ubik_value *value,
        struct ubik_value *type)
{
        ubik_rwlock_write_scope(&env->lock);
        return _set(env, uri, value, type, true);
}

no_ignore ubik_error
ubik_env_watch(
        ubik_env_cb callback,
        struct ubik_env *env,
        struct ubik_uri *uri,
        void *callback_arg)
{
        struct ubik_env_watch *watcher;
        struct ubik_env_watch_list *watchlist;
        ubik_rwlock_write_scope(&env->lock);

        watcher = calloc(1, sizeof(struct ubik_env_watch));
        if (watcher == NULL)
                return ubik_raise(ERR_NO_MEMORY, "env watch alloc");

        watcher->uri = uri;
        watcher->cb = callback;
        watcher->arg = callback_arg;
        watcher->target_env = env;
        watcher->fired = false;

        while (env != NULL)
        {
                watchlist = calloc(1, sizeof(struct ubik_env_watch_list));
                if (watchlist == NULL)
                        return ubik_raise(ERR_NO_MEMORY, "env watchlist alloc");

                watchlist->watch = watcher;
                watchlist->prev = env->watches;
                watchlist->next = NULL;
                if (env->watches != NULL)
                        env->watches->next = watchlist;
                env->watches = watchlist;

                watcher->refcount++;
                env = env->parent;
        }

        return OK;
}
