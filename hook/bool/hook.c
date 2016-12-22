/*
 * bool/hook.c: boolean constructors
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

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include "ubik/assert.h"
#include "ubik/hooks.h"
#include "ubik/rt.h"
#include "ubik/rttypes.h"
#include "ubik/schedule.h"

static ubik_error
native_true(struct ubik_exec_graph *gexec)
{
        struct ubik_value *res;
        struct ubik_value *type_decl;
        ubik_error err;

        err = ubik_value_new(&res, gexec->workspace);
        if (err != OK)
                return err;
        res->gc.runtime_managed = true;
        err = ubik_value_new(&type_decl, gexec->workspace);
        if (err != OK)
                return err;
        type_decl->gc.runtime_managed = true;

        res->type = UBIK_BOO;
        res->boo.value = true;

        err = ubik_type_boo(type_decl);
        if (err != OK)
                return err;
        gexec->nv[gexec->v->fun.result] = res;
        gexec->nt[gexec->v->fun.result] = type_decl;
        return OK;
}

static ubik_error
native_false(struct ubik_exec_graph *gexec)
{
        struct ubik_value *res;
        struct ubik_value *type_decl;
        ubik_error err;

        err = ubik_value_new(&res, gexec->workspace);
        if (err != OK)
                return err;
        res->gc.runtime_managed = true;
        err = ubik_value_new(&type_decl, gexec->workspace);
        if (err != OK)
                return err;
        type_decl->gc.runtime_managed = true;

        res->type = UBIK_BOO;
        res->boo.value = false;

        err = ubik_type_boo(type_decl);
        if (err != OK)
                return err;
        gexec->nv[gexec->v->fun.result] = res;
        gexec->nt[gexec->v->fun.result] = type_decl;
        return OK;
}

#define rcast (struct ubik_native_record)

ubik_error
__ubik_install(struct ubik_vector *hooks, struct ubik_alloc_region *region)
{
        struct ubik_native_record *r;
        ubik_error err;

        ubik_alloc1(&r, struct ubik_native_record, region);
        *r = rcast {
                "ubik-native-boolean-true", 0, "Boolean", NULL, native_true
        };
        err = ubik_vector_append(hooks, r);
        if (err != OK)
                return err;

        ubik_alloc1(&r, struct ubik_native_record, region);
        *r = rcast {
                "ubik-native-boolean-false", 0, "Boolean", NULL, native_false
        };
        err = ubik_vector_append(hooks, r);
        if (err != OK)
                return err;

        return OK;
}
