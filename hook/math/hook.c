/*
 * math/hook.c: built-in native arithmetic methods
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
#include "ubik/hooks.h"
#include "ubik/rat.h"
#include "ubik/rttypes.h"
#include "ubik/ubik.h"
#include "ubik/util.h"

DEF_EVALUATOR(add)
{
        struct ubik_value *res;
        ubik_error err;

        err = ubik_value_new(&res, ws);
        if (err != OK)
                return err;

        ubik_rat_add(res, args[0], args[1]);
        *res_ref = res;
        *res_type = argtypes[0];

        return OK;
}

DEF_EVALUATOR(subtract)
{
        struct ubik_value *res;
        ubik_error err;

        err = ubik_value_new(&res, ws);
        if (err != OK)
                return err;

        ubik_rat_sub(res, args[0], args[1]);
        *res_ref = res;
        *res_type = argtypes[0];

        return OK;
}

DEF_EVALUATOR(multiply)
{
        struct ubik_value *res;
        ubik_error err;

        err = ubik_value_new(&res, ws);
        if (err != OK)
                return err;

        ubik_rat_mul(res, args[0], args[1]);
        *res_ref = res;
        *res_type = argtypes[0];

        return OK;
}

DEF_EVALUATOR(divide)
{
        struct ubik_value *res;
        ubik_error err;

        err = ubik_value_new(&res, ws);
        if (err != OK)
                return err;

        ubik_rat_div(res, args[0], args[1]);
        *res_ref = res;
        *res_type = argtypes[0];

        return OK;
}

DEF_EVALUATOR(remain)
{
        struct ubik_value *res;
        ubik_error err;

        err = ubik_value_new(&res, ws);
        if (err != OK)
                return err;

        ubik_rat_mod(res, args[0], args[1]);
        *res_ref = res;
        *res_type = argtypes[0];

        return OK;
}

DEF_EVALUATOR(less_than)
{
        ubik_error err;

        err = ubik_value_new(res_ref, ws);
        if (err != OK)
                return err;
        err = ubik_value_new(res_type, ws);
        if (err != OK)
                return err;

        (*res_ref)->type = UBIK_BOO;
        (*res_ref)->boo.value = ubik_rat_lt(args[0], args[1]);

        err = ubik_type_boo(*res_type);
        if (err != OK)
                return err;

        return OK;
}

ubik_error
__ubik_install(struct ubik_vector *hooks, struct ubik_alloc_region *region)
{
        struct ubik_hook *r;
        ubik_error err;

        ubik_alloc1(&r, struct ubik_hook, region);
        *r = (struct ubik_hook) { "+", 2, "Number -> Number -> Number", NULL, add };
        err = ubik_vector_append(hooks, r);
        if (err != OK)
                return err;

        ubik_alloc1(&r, struct ubik_hook, region);
        *r = (struct ubik_hook) { "-", 2, "Number -> Number -> Number", NULL, subtract };
        err = ubik_vector_append(hooks, r);
        if (err != OK)
                return err;

        ubik_alloc1(&r, struct ubik_hook, region);
        *r = (struct ubik_hook) { "*", 2, "Number -> Number -> Number", NULL, multiply };
        err = ubik_vector_append(hooks, r);
        if (err != OK)
                return err;

        ubik_alloc1(&r, struct ubik_hook, region);
        *r = (struct ubik_hook) { "/", 2, "Number -> Number -> Number", NULL, divide };
        err = ubik_vector_append(hooks, r);
        if (err != OK)
                return err;

        ubik_alloc1(&r, struct ubik_hook, region);
        *r = (struct ubik_hook) { "%", 2, "Number -> Number -> Number", NULL, remain };
        err = ubik_vector_append(hooks, r);
        if (err != OK)
                return err;

        ubik_alloc1(&r, struct ubik_hook, region);
        *r = (struct ubik_hook) {
                "ubik-native-lt", 2, "Number -> Number -> Boolean", NULL, less_than };
        err = ubik_vector_append(hooks, r);
        if (err != OK)
                return err;

        return OK;
}
