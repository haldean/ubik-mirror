/*
 * rt.c: expel runtime
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

#include "expel/env.h"
#include "expel/expel.h"
#include "expel/gc.h"
#include "expel/natives.h"

xl_error
xl_start()
{
        xl_error err;

        xl_gc_start();

        err = xl_natives_register(xl_env_get_root());
        if (err != OK)
                return err;

        return OK;
}

xl_error
xl_teardown()
{
        xl_error err;

        err = xl_env_free(xl_env_get_root());
        if (err != OK)
                return err;

        xl_gc_teardown();

        return OK;
}
