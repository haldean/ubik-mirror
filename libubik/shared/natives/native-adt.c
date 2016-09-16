/*
 * native-adt.c: built-in native methods for ADTs
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

#include "ubik/adt.h"
#include "ubik/assert.h"
#include "ubik/env.h"
#include "ubik/list.h"
#include "ubik/natives.h"
#include "ubik/rttypes.h"
#include "ubik/schedule.h"
#include "ubik/ubik.h"
#include "ubik/util.h"
#include "ubik/value.h"

#include <string.h>

static ubik_error
_native_adt_new(struct ubik_exec_graph *gexec)
{
        struct ubik_value *type_decl;
        struct ubik_value *ctor;
        struct ubik_value *args;
        struct ubik_value *res;
        struct ubik_value *graph;
        ubik_error err;
        size_t i;

        ubik_assert(gexec->v->type == UBIK_PAP);
        graph = gexec->v->pap.base_func;

        type_decl = gexec->nv[0];
        ctor = gexec->nv[1];

        err = ubik_value_new(&args, gexec->workspace);
        if (err != OK)
                return err;
        err = ubik_list_create_empty(args);
        if (err != OK)
                return err;

        for (i = 2; i < graph->fun.n - 1; i++)
        {
                err = ubik_list_append(
                        args, gexec->nv[i], gexec->workspace);
                if (err != OK)
                        return err;
        }

        err = ubik_value_new(&res, gexec->workspace);
        if (err != OK)
                return err;
        err = ubik_adt_instantiate(
                res, type_decl, ctor, args, gexec->workspace);
        if (err != OK)
                return err;
        gexec->nv[graph->fun.result] = res;
        gexec->nt[graph->fun.result] = type_decl;
        return OK;
}

ubik_error
_register_all_adt_new(struct ubik_env *env, struct ubik_workspace *ws)
{
        struct ubik_value *graph;
        struct ubik_uri *uri;
        struct ubik_value *type;
        ubik_error err;
        char *func_name;
        int res;
        int i;

        for (i = 0; i < UBIK_MAX_ADT_FIELDS; i++)
        {
                graph = NULL;
                err = ubik_internal_native_create_op(
                        &graph, i + 2, _native_adt_new, ws);
                if (err != OK)
                        return err;

                res = asprintf(&func_name, "ubik-adt-new-%d", i);
                if (res < 0)
                        return ubik_raise(ERR_NO_MEMORY, "adt new name alloc");
                err = ubik_internal_native_uri(&uri, func_name);
                if (err != OK)
                        return err;
                free(func_name);

                err = ubik_value_new(&type, ws);
                if (err != OK)
                        return err;

                err = ubik_env_set(env, uri, graph, type);
                if (err != OK)
                        return err;
        }

        return OK;
}

no_ignore static ubik_error
_native_adt_ctor_matches(struct ubik_exec_graph *gexec)
{
        struct ubik_value *inst;
        struct ubik_value *match_name;
        struct ubik_value *ctor_name;
        struct ubik_value *res;
        struct ubik_value *res_type;
        bool matches;
        ubik_error err;

        ubik_assert(gexec->v->type == UBIK_PAP);

        match_name = gexec->nv[0];
        inst = gexec->nv[1];

        err = ubik_adt_get_ctor(&ctor_name, inst);
        if (err != OK)
                return err;

        matches = ubik_value_eq(match_name, ctor_name);

        err = ubik_value_new(&res, gexec->workspace);
        if (err != OK)
                return err;
        res->type = UBIK_BOO;
        res->boo.value = matches;
        gexec->nv[gexec->v->pap.base_func->fun.result] = res;

        err = ubik_value_new(&res_type, gexec->workspace);
        if (err != OK)
                return err;
        err = ubik_type_bool(res_type);
        if (err != OK)
                return err;
        gexec->nt[gexec->v->pap.base_func->fun.result] = res_type;

        return OK;
}

ubik_error
_register_adt_ctor_matches(struct ubik_env *env, struct ubik_workspace *ws)
{
        struct ubik_value *graph;
        struct ubik_uri *uri;
        struct ubik_value *type;
        ubik_error err;

        graph = NULL;
        err = ubik_internal_native_create_op(
                &graph, 2, _native_adt_ctor_matches, ws);
        if (err != OK)
                return err;

        err = ubik_internal_native_uri(&uri, "ubik-adt-ctor-matches?");
        if (err != OK)
                return err;

        err = ubik_value_new(&type, ws);
        if (err != OK)
                return err;

        err = ubik_env_set(env, uri, graph, type);
        if (err != OK)
                return err;

        return OK;
}

no_ignore static ubik_error
_native_adt_get(struct ubik_exec_graph *gexec)
{
        struct ubik_value *inst;
        struct ubik_value *index_val;
        struct ubik_value *res;
        struct ubik_value *res_type;
        ubik_word index;
        ubik_error err;

        index_val = gexec->nv[0];
        inst = gexec->nv[1];

        ubik_assert(index_val->type == UBIK_RAT && index_val->rat.den == 1);
        index = index_val->rat.num;

        err = ubik_adt_get_field(&res, inst, index);
        if (err != OK)
                return err;
        err = ubik_adt_get_field_type(&res_type, inst, index);
        if (err != OK)
                return err;

        ubik_assert(gexec->v->type == UBIK_PAP);
        gexec->nv[gexec->v->pap.base_func->fun.result] = res;
        gexec->nt[gexec->v->pap.base_func->fun.result] = res_type;

        return OK;
}

ubik_error
_register_adt_get(struct ubik_env *env, struct ubik_workspace *ws)
{
        struct ubik_value *graph;
        struct ubik_uri *uri;
        struct ubik_value *type;
        ubik_error err;

        graph = NULL;
        err = ubik_internal_native_create_op(&graph, 2, _native_adt_get, ws);
        if (err != OK)
                return err;

        err = ubik_internal_native_uri(&uri, "ubik-adt-get");
        if (err != OK)
                return err;

        err = ubik_value_new(&type, ws);
        if (err != OK)
                return err;

        err = ubik_env_set(env, uri, graph, type);
        if (err != OK)
                return err;

        return OK;
}
