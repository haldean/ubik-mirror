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
#include "ubik/schedule.h"
#include "ubik/ubik.h"
#include "ubik/util.h"

ubik_error
add(struct ubik_exec_graph *gexec)
{
        struct ubik_value *res;
        ubik_error err;

        err = ubik_value_new(&res, gexec->workspace);
        if (err != OK)
                return err;

        ubik_rat_add(res, gexec->nv[0], gexec->nv[1]);
        gexec->nv[2] = res;
        gexec->nt[2] = gexec->nt[0];

        return OK;
}

ubik_error
subtract(struct ubik_exec_graph *gexec)
{
        struct ubik_value *res;
        ubik_error err;

        err = ubik_value_new(&res, gexec->workspace);
        if (err != OK)
                return err;

        ubik_rat_sub(res, gexec->nv[0], gexec->nv[1]);
        gexec->nv[2] = res;
        gexec->nt[2] = gexec->nt[0];

        return OK;
}

ubik_error
multiply(struct ubik_exec_graph *gexec)
{
        struct ubik_value *res;
        ubik_error err;

        err = ubik_value_new(&res, gexec->workspace);
        if (err != OK)
                return err;

        ubik_rat_mul(res, gexec->nv[0], gexec->nv[1]);
        gexec->nv[2] = res;
        gexec->nt[2] = gexec->nt[0];

        return OK;
}

ubik_error
divide(struct ubik_exec_graph *gexec)
{
        struct ubik_value *res;
        ubik_error err;

        err = ubik_value_new(&res, gexec->workspace);
        if (err != OK)
                return err;

        ubik_rat_div(res, gexec->nv[0], gexec->nv[1]);
        gexec->nv[2] = res;
        gexec->nt[2] = gexec->nt[0];

        return OK;
}

ubik_error
remain(struct ubik_exec_graph *gexec)
{
        struct ubik_value *res;
        ubik_error err;

        err = ubik_value_new(&res, gexec->workspace);
        if (err != OK)
                return err;

        ubik_rat_mod(res, gexec->nv[0], gexec->nv[1]);
        gexec->nv[2] = res;
        gexec->nt[2] = gexec->nt[0];

        return OK;
}

ubik_error
less_than(struct ubik_exec_graph *gexec)
{
        ubik_error err;

        err = ubik_value_new(&gexec->nv[2], gexec->workspace);
        if (err != OK)
                return err;
        err = ubik_value_new(&gexec->nt[2], gexec->workspace);
        if (err != OK)
                return err;

        gexec->nv[2]->type = UBIK_BOO;
        gexec->nv[2]->boo.value = ubik_rat_lt(gexec->nv[0], gexec->nv[1]);

        err = ubik_type_boo(gexec->nt[2]);
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
