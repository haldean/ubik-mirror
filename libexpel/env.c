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

#include "expel/expel.h"
#include "expel/env.h"
#include "expel/util.h"

#define ENV_MAX_LOAD 0.5
#define ENV_INIT_CAP 8
#define ENV_CAP_SCALE 2

no_ignore xl_error_t
xl_env_init(struct xl_env *env)
{
        env->bindings = NULL;
        env->n = 0;
        env->cap = 0;
        return OK;
}

no_ignore xl_error_t
xl_env_free(struct xl_env *env)
{
        xl_error_t err;
        size_t i;

        if (likely(env->bindings != NULL))
        {
                for (i = 0; i < env->cap; i++)
                {
                        if (env->bindings[i].value == NULL)
                                continue;
                        err = xl_release(env->bindings[i].value);
                        if (err != OK)
                                return err;
                }
                free(env->bindings);
        }

        env->n = 0;
        env->cap = 0;
        return OK;
}

no_ignore xl_error_t
xl_get(
        struct xl_value **value,
        struct xl_value **type,
        struct xl_env *env,
        struct xl_uri *uri)
{
        size_t i;
        size_t probed;
        bool found;

        i = uri->hash % env->cap;
        for (probed = 0; probed < env->cap; probed++)
        {
                if (env->bindings[i].uri->hash != uri->hash)
                        continue;
                if (xl_uri_eq(uri, env->bindings[i].uri))
                {
                        *value = env->bindings[i].value;
                        *type = env->bindings[i].type;
                        found = true;
                        break;
                }
                i = (i + 1) % env->cap;
        }

        return found ? OK : xl_raise(ERR_ABSENT, "xl_get");
}

/* Inserts the given URI-value pair into the given binding array.
 *
 * This assumes that the table has space for another entry, and
 * will return ERR_FULL if the table has no spaces remaining.
 *
 * The overwrite parameter controls whether existing data will be
 * overwritten. True means that it will be, false means that it
 * will not. */
no_ignore static xl_error_t
__insert(
        struct xl_binding *binds,
        size_t cap,
        struct xl_uri *uri,
        struct xl_value *value,
        struct xl_value *type,
        bool overwrite)
{
        size_t i;
        size_t probed;
        xl_error_t err;

        i = uri->hash % cap;
        for (probed = 0; probed < cap; probed++)
        {
                if (binds[i].uri == NULL)
                        break;
                if (xl_uri_eq(binds[i].uri, uri))
                        break;
                i = (i + 1) % cap;
        }
        if (unlikely(probed == cap))
                return xl_raise(ERR_FULL, "env insert");

        /* There was already a value at this key, we need to release our
         * reference on it. */
        err = OK;
        if (unlikely(binds[i].value != NULL))
        {
                if (!overwrite)
                        return xl_raise(ERR_PRESENT, "env overwrite");
                err = xl_release(binds[i].value);
        }

        if (err == OK)
        {
                binds[i].uri = uri;
                binds[i].value = value;
                binds[i].type = type;
        }
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
static xl_error_t
__resize_rebalance(struct xl_env *env)
{
        struct xl_binding *new_binds;
        size_t new_cap;
        size_t i;
        size_t reinserted;
        xl_error_t err;

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

                err = __insert(
                        new_binds,
                        new_cap,
                        env->bindings[i].uri,
                        env->bindings[i].value,
                        env->bindings[i].type,
                        false);
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

no_ignore static xl_error_t
__set(
        struct xl_env *env,
        struct xl_uri *uri,
        struct xl_value *value,
        struct xl_value *type,
        bool overwrite)
{
        struct xl_uri *uri_copied;
        xl_error_t err, ignore;

        err = OK;
        if (unlikely(env->cap == 0))
                err = __resize_rebalance(env);
        else if (unlikely((float) env->n / (float) env->cap > ENV_MAX_LOAD))
                err = __resize_rebalance(env);
        if (err != OK)
                return err;

        /* copy the URI into a new struct to avoid post-modification bugs. */
        uri_copied = malloc(sizeof(struct xl_uri));
        memcpy(uri_copied, uri, sizeof(struct xl_uri));

        /* Take a reference to the value. We do this before we know whether the
         * insert succeeded, because the insert can result in a release, which
         * itself can result in a GC. We want to make sure that this doesn't get
         * GCed if we are going to keep this thing, so we take a reference now
         * and release it if the insert fails later. */
        err = xl_take(value);
        if (err != OK)
                return err;

        err = __insert(env->bindings, env->cap, uri_copied, value,
                       type, overwrite);
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
                ignore = xl_release(value);
                unused(ignore);
        }

        return err;
}

no_ignore xl_error_t
xl_set(
        struct xl_env *env,
        struct xl_uri *uri,
        struct xl_value *value,
        struct xl_value *type)
{
        return __set(env, uri, value, type, false);
}

no_ignore xl_error_t
xl_overwrite(
        struct xl_env *env,
        struct xl_uri *uri,
        struct xl_value *value,
        struct xl_value *type)
{
        return __set(env, uri, value, type, true);
}
