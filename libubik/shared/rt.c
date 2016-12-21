/*
 * rt.c: ubik runtime
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

#include "ubik/alloc.h"
#include "ubik/env.h"
#include "ubik/natives.h"
#include "ubik/string.h"
#include "ubik/ubik.h"

/* these are used so that we can bit-shift them and still have a constexpr for
   UBIK_VERSION. */
#define MAJOR 0
#define MINOR 0
#define PATCH 0

const uint16_t UBIK_MAJOR = MAJOR;
const uint16_t UBIK_MINOR = MINOR;
const uint32_t UBIK_PATCH = PATCH;
const uint64_t UBIK_VERSION =
        (uint64_t) MAJOR << 48 | (uint64_t) MINOR << 32 | PATCH;

ubik_error
ubik_start(struct ubik_workspace *ws)
{
        ubik_local_region(r);
        ubik_error err;
        char *hook_root;
        char *hook_path;

        hook_root = getenv("UBIK_HOOK_ROOT");
        if (hook_root == NULL)
                hook_root = "hook";

        err = ubik_string_path_concat(
                &hook_path, hook_root, "emit.so", &r);
        if (err != OK)
                return err;

        err = ubik_natives_load_hook(hook_path);
        if (err != OK)
                return err;

        err = ubik_natives_register(ubik_env_get_root(), ws);
        if (err != OK)
                return err;

        err = ubik_natives_cache_types();
        if (err != OK)
                return err;

        return OK;
}

ubik_error
ubik_teardown()
{
        ubik_error err;

        err = ubik_env_free(ubik_env_get_root());
        if (err != OK)
                return err;

        ubik_natives_teardown();
        return OK;
}
