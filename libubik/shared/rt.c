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

#include "ubik/env.h"
#include "ubik/ubik.h"
#include "ubik/gc.h"
#include "ubik/natives.h"

ubik_error
ubik_start()
{
        ubik_error err;

        ubik_gc_start();

        err = ubik_natives_register(ubik_env_get_root());
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

        ubik_gc_teardown();

        return OK;
}
