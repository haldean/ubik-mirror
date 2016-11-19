/*
 * native-bool.c: boolean constructors
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
#include "ubik/natives.h"
#include "ubik/rt.h"
#include "ubik/rttypes.h"
#include "ubik/schedule.h"

static ubik_error
_native_bool_true(struct ubik_exec_graph *gexec)
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
        gexec->nv[gexec->v->pap.base_func->fun.result] = res;
        gexec->nt[gexec->v->pap.base_func->fun.result] = type_decl;
        return OK;
}

static ubik_error
_native_bool_false(struct ubik_exec_graph *gexec)
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
        gexec->nv[gexec->v->pap.base_func->fun.result] = res;
        gexec->nt[gexec->v->pap.base_func->fun.result] = type_decl;
        return OK;
}

ubik_error
_register_boolean_true(struct ubik_env *env, struct ubik_workspace *ws)
{
        struct ubik_value *graph;
        struct ubik_uri *uri;
        struct ubik_value *type;
        ubik_error err;

        graph = NULL;
        err = ubik_internal_native_create_op(
                &graph, 2, _native_bool_true, ws);
        if (err != OK)
                return err;

        err = ubik_value_new(&type, ws);
        if (err != OK)
                return err;
        type->gc.runtime_managed = true;
        type->type = UBIK_TYP;

        err = ubik_internal_native_uri(&uri, "ubik-native-boolean-true");
        if (err != OK)
                return err;

        err = ubik_env_set(env, uri, graph, type);
        ubik_uri_free(uri);
        if (err != OK)
                return err;

        return OK;
}

ubik_error
_register_boolean_false(struct ubik_env *env, struct ubik_workspace *ws)
{
        struct ubik_value *graph;
        struct ubik_uri *uri;
        struct ubik_value *type;
        ubik_error err;

        graph = NULL;
        err = ubik_internal_native_create_op(
                &graph, 2, _native_bool_false, ws);
        if (err != OK)
                return err;

        err = ubik_value_new(&type, ws);
        if (err != OK)
                return err;
        type->gc.runtime_managed = true;
        type->type = UBIK_TYP;

        err = ubik_internal_native_uri(&uri, "ubik-native-boolean-false");
        if (err != OK)
                return err;

        err = ubik_env_set(env, uri, graph, type);
        ubik_uri_free(uri);
        if (err != OK)
                return err;

        return OK;
}
