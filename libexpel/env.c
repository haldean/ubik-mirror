/*
 * env.c: expel environment definitions
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

#include "expel/assert.h"
#include "expel/expel.h"
#include "expel/env.h"
#include "expel/util.h"

#define ENV_MAX_LOAD 0.5
#define ENV_INIT_CAP 8
#define ENV_CAP_SCALE 2

static struct xl_env root;

struct xl_env *
xl_env_get_root()
{
        return &root;
}

no_ignore xl_error
xl_env_init(struct xl_env *env)
{
        env->bindings = NULL;
        env->n = 0;
        env->cap = 0;
        env->parent = xl_env_get_root();
        return OK;
}

no_ignore xl_error
xl_env_make_child(struct xl_env *child, struct xl_env *parent)
{
        xl_error err;
        err = xl_env_init(child);
        if (err != OK)
                return err;
        child->parent = parent;
        return OK;
}

no_ignore xl_error
xl_env_free(struct xl_env *env)
{
        xl_error err;
        size_t i;

        if (likely(env->bindings != NULL))
        {
                for (i = 0; i < env->cap; i++)
                {
                        if (env->bindings[i].value.any == NULL)
                                continue;
                        err = xl_release(env->bindings[i].uri);
                        if (err != OK)
                                return err;
                        err = xl_release(env->bindings[i].type);
                        if (err != OK)
                                return err;
                        err = xl_release(env->bindings[i].value.any);
                        if (err != OK)
                                return err;
                }
                free(env->bindings);
        }

        env->n = 0;
        env->cap = 0;
        env->bindings = NULL;
        if (env != xl_env_get_root())
                env->parent = xl_env_get_root();
        return OK;
}

no_ignore xl_error
xl_get(
        union xl_value_or_graph *value,
        struct xl_value **type,
        xl_word *value_type,
        struct xl_env *env,
        struct xl_uri *uri)
{
        size_t i;
        size_t probed;
        bool found;

        found = false;

        if (env->cap != 0)
        {
                i = uri->hash % env->cap;
                for (probed = 0; probed < env->cap; probed++)
                {
                        if (env->bindings[i].uri == NULL)
                                break;
                        if (env->bindings[i].uri->hash != uri->hash)
                                continue;
                        if (xl_uri_eq(uri, env->bindings[i].uri))
                        {
                                value->any = env->bindings[i].value.any;
                                *value_type = env->bindings[i].value_type;
                                *type = env->bindings[i].type;
                                found = true;
                                break;
                        }
                        i = (i + 1) % env->cap;
                }
        }

        if (found)
                return OK;

        xl_assert(env->parent != env);
        if (env->parent == NULL)
                return xl_raise(ERR_ABSENT, "xl_get");
        return xl_get(value, type, value_type, env->parent, uri);
}

/* Inserts the given URI-value pair into the given binding array.
 *
 * This assumes that the table has space for another entry, and
 * will return ERR_FULL if the table has no spaces remaining.
 *
 * The overwrite parameter controls whether existing data will be
 * overwritten. True means that it will be, false means that it
 * will not. */
no_ignore static xl_error
_insert(
        struct xl_binding *binds,
        size_t cap,
        struct xl_binding *insert,
        bool overwrite)
{
        size_t i;
        size_t probed;
        xl_error err;

        i = insert->uri->hash % cap;
        for (probed = 0; probed < cap; probed++)
        {
                if (binds[i].uri == NULL)
                        break;
                if (xl_uri_eq(binds[i].uri, insert->uri))
                        break;
                i = (i + 1) % cap;
        }
        if (unlikely(probed == cap))
                return xl_raise(ERR_FULL, "env insert");

        /* There was already a value at this key, we need to release our
         * reference on it. */
        err = OK;
        if (unlikely(binds[i].value.any != NULL))
        {
                if (!overwrite)
                        return xl_raise(ERR_PRESENT, "env overwrite");
                err = xl_release(binds[i].value.any);
        }

        if (err == OK)
                binds[i] = *insert;
        return err;
}

/* Resizes and rebalances an environment, scaling the capacity by
 * ENV_CAP_SCALE as defined above.
 *
 * This first allocates a new bindings array, and then reinserts the
 * bindings from the old bindings array into the new one. Finally, it
 * frees the old array and updates the env struct to reference the new
 * array. If an error occurs during rebalancing, the environment remains
 * unmodified and a nonzero error code is returned. */
static xl_error
_resize_rebalance(struct xl_env *env)
{
        struct xl_binding *new_binds;
        size_t new_cap;
        size_t i;
        size_t reinserted;
        xl_error err;

        if (env->cap == 0)
                new_cap = ENV_INIT_CAP;
        else
                new_cap = ENV_CAP_SCALE * env->cap;

        new_binds = calloc(new_cap, sizeof(struct xl_binding));
        if (new_binds == NULL)
                return xl_raise(ERR_NO_MEMORY, "env resize");

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

no_ignore static xl_error
_set(
        struct xl_env *env,
        struct xl_uri *uri,
        union xl_value_or_graph value,
        struct xl_value *type,
        xl_word value_type,
        bool overwrite)
{
        struct xl_binding new_binding;
        xl_error err, ignore;

        err = OK;
        if (unlikely(env->cap == 0))
                err = _resize_rebalance(env);
        else if (unlikely((float) env->n / (float) env->cap > ENV_MAX_LOAD))
                err = _resize_rebalance(env);
        if (err != OK)
                return err;

        new_binding.uri = uri;
        new_binding.value = value;
        new_binding.value_type = value_type;
        new_binding.type = type;

        /* Take a reference to the value. We do this before we know whether the
         * insert succeeded, because the insert can result in a release, which
         * itself can result in a GC. We want to make sure that this doesn't get
         * GCed if we are going to keep this thing, so we take a reference now
         * and release it if the insert fails later. */
        err = xl_take(value.any);
        if (err != OK)
                return err;
        err = xl_take(type);
        if (err != OK)
                return err;
        err = xl_take(uri);
        if (err != OK)
                return err;

        err = _insert(env->bindings, env->cap, &new_binding, overwrite);
        if (err == OK)
                env->n++;
        else
        {
                /* We're on the clean-up codepath, and we want the returned
                 * error to be the actual error, not whatever went wrong during
                 * the release, so we drop this error on the ground.
                 *
                 * (I like that it takes this much effort to ignore an
                 * unignorable parameter) */
                ignore = xl_release(value.any);
                unused(ignore);
                ignore = xl_release(type);
                unused(ignore);
                ignore = xl_release(uri);
                unused(ignore);
        }

        return err;
}

no_ignore xl_error
xl_set(
        struct xl_env *env,
        struct xl_uri *uri,
        union xl_value_or_graph value,
        struct xl_value *type,
        xl_word value_type)
{
        return _set(env, uri, value, type, value_type, false);
}

no_ignore xl_error
xl_overwrite(
        struct xl_env *env,
        struct xl_uri *uri,
        union xl_value_or_graph value,
        struct xl_value *type,
        xl_word value_type)
{
        return _set(env, uri, value, type, value_type, true);
}
