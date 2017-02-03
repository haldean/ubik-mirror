/*
 * emit/hook.c: stdout support
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

#include <stdio.h>
#include "ubik/hooks.h"
#include "ubik/rt.h"

DEF_EVALUATOR(emit)
{
        /* this uses fwrite to remove the requirement for a NULL byte at the end
           of the string. */
        fwrite(args[0]->str.data, sizeof(char), args[0]->str.length, stdout);

        *res_ref = args[0];
        *res_type = argtypes[0];
        return OK;
}

ubik_error
__ubik_install(struct ubik_vector *hooks, struct ubik_alloc_region *region)
{
        struct ubik_hook *r;
        ubik_alloc1(&r, struct ubik_hook, region);
        *r = (struct ubik_hook) { "emit", 1, "String -> String", NULL, emit };
        return ubik_vector_append(hooks, r);
}
