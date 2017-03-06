/*
 * bool/string.c: string operations
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

#include "ubik/assert.h"
#include "ubik/env.h"
#include "ubik/hooks.h"
#include "ubik/rttypes.h"
#include "ubik/str.h"
#include "ubik/ubik.h"
#include "ubik/util.h"
#include "ubik/value.h"

DEF_EVALUATOR(concat)
{
        struct ubik_value *res;
        ubik_error err;

        err = ubik_value_new(&res, ws);
        if (err != OK)
                return err;

        if (args[0]->type != UBIK_STR)
                return ubik_raise(ERR_BAD_TYPE, "concat value was not a str");
        if (args[1]->type != UBIK_STR)
                return ubik_raise(ERR_BAD_TYPE, "concat value was not a str");
        ubik_str_concat(res, args[0], args[1]);
        *res_ref = res;
        *res_type = argtypes[0];

        return OK;
}

DEF_EVALUATOR(humanize)
{
        struct ubik_value *res;
        struct ubik_value *type;
        ubik_error err;
        char *str;
        size_t str_size;

        err = ubik_value_new(&res, ws);
        if (err != OK)
                return err;
        err = ubik_value_new(&type, ws);
        if (err != OK)
                return err;

        err = ubik_value_humanize(&str, &str_size, args[0]);
        if (err != OK)
                return err;
        res->type = UBIK_STR;
        res->str.data = str;
        res->str.length = str_size;

        err = ubik_type_str(type);
        if (err != OK)
                return err;

        *res_ref = res;
        *res_type = type;
        return OK;
}

ubik_error
__ubik_install(struct ubik_vector *hooks, struct ubik_alloc_region *region)
{
        struct ubik_hook *r;
        ubik_error err;

        ubik_alloc1(&r, struct ubik_hook, region);
        *r = (struct ubik_hook) {
                "concat", 2, "ubik:String -> ubik:String -> ubik:String",
                NULL, concat };
        err = ubik_vector_append(hooks, r);
        if (err != OK)
                return err;

        ubik_alloc1(&r, struct ubik_hook, region);
        *r = (struct ubik_hook) {
                "humanize", 1, "ubik:a -> ubik:String", NULL, humanize };
        err = ubik_vector_append(hooks, r);
        if (err != OK)
                return err;

        return OK;
}
