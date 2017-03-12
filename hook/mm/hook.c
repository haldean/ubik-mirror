/*
 * mm/hook.c: multimethod support
 * Copyright (C) 2017, Haldean Brown
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

#include <inttypes.h>
#include <stdio.h>
#include <string.h>

#include "ubik/alloc.h"
#include "ubik/hooks.h"
#include "ubik/rt.h"
#include "ubik/string.h"

DEF_EVALUATOR(mm_call)
{
        /* this uses fwrite to remove the requirement for a NULL byte at the end
           of the string. */
        fwrite(args[0]->str.data, sizeof(char), args[0]->str.length, stdout);

        *res_ref = args[0];
        *res_type = argtypes[0];
        return OK;
}

#define TYPEBUF_SIZE 2048

ubik_error
__ubik_install(struct ubik_vector *hooks, struct ubik_alloc_region *region)
{
        struct ubik_hook *r;
        size_t i;
        size_t j;
        char typebuf[TYPEBUF_SIZE];
        char *funcname;
        ubik_error err;

        for (i = 1; i < UBIK_MAX_FUNC_PARAMS; i++)
        {
                memset(typebuf, 0x00, TYPEBUF_SIZE);
                strcat(typebuf, "ubik:uri-type");
                for (j = 0; j < i; j++)
                {
                        sprintf(&typebuf[strlen(typebuf)],
                                " -> ubik:arg-%" PRIu64, j);
                }
                strcat(typebuf, " -> ubik:ret-type");

                ubik_asprintf(&funcname, region,
                              "ubik-multimethod-call-%" PRIu64, i);

                ubik_alloc1(&r, struct ubik_hook, region);
                *r = (struct ubik_hook) {
                        funcname, i + 1, ubik_strdup(typebuf, region),
                        NULL, mm_call };
                err = ubik_vector_append(hooks, r);
                if (err != OK)
                        return err;
        }
        return OK;
}

