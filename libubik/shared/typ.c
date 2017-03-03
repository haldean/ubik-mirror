/*
 * typ.c: utilities for working with TYP objects
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
#include <stdlib.h>
#include <string.h>

#include "ubik/rt.h"
#include "ubik/typ.h"
#include "ubik/util.h"

no_ignore static ubik_error
adt_typ(
        struct ubik_value **res_ptr,
        struct ubik_type *t,
        struct ubik_typesystem *tsys,
        struct ubik_workspace *ws)
{
        struct ubik_type_params *src_params;
        struct ubik_type_constraints *src_constraints;
        struct ubik_ast_adt_ctors *src_ctors;
        struct ubik_type_list *src_ctor_param;
        struct ubik_type_expr *argtype;
        struct ubik_value *res;
        struct ubik_value *t0;
        ubik_error err;
        size_t i;
        size_t j;
        size_t n_vars;
        unused(ws);

        err = ubik_value_new(&res, ws);
        if (err != OK)
                return err;

        src_constraints = t->adt.constraints;
        if (src_constraints != NULL)
                return ubik_raise(
                        ERR_NOT_IMPLEMENTED, "no constraints on ADTs yet");

        res->type = UBIK_TYP;
        res->typ.t = UBIK_TYPE_ADT;
        res->typ.adt.n_ctors = 0;

        for (src_params = t->adt.params;
             src_params != NULL; src_params = src_params->next)
                res->typ.adt.n_params++;
        res->typ.adt.params = calloc(
                res->typ.adt.n_params, sizeof(struct ubik_str));

        for (src_ctors = t->adt.ctors;
             src_ctors != NULL; src_ctors = src_ctors->next)
                res->typ.adt.n_ctors++;
        res->typ.adt.ctors = calloc(
                res->typ.adt.n_ctors, sizeof(struct ubik_typ_ctor));

        for (src_ctors = t->adt.ctors, i = 0;
             src_ctors != NULL && i < res->typ.adt.n_ctors;
             src_ctors = src_ctors->next, i++)
        {
                res->typ.adt.ctors[i].name.data = strdup(src_ctors->name);
                res->typ.adt.ctors[i].name.length = strlen(src_ctors->name);

                res->typ.adt.ctors[i].arity = 0;
                for (src_ctor_param = src_ctors->params;
                     src_ctor_param != NULL;
                     src_ctor_param = src_ctor_param->next)
                        res->typ.adt.ctors[i].arity++;

                res->typ.adt.ctors[i].arg_types = calloc(
                        res->typ.adt.ctors[i].arity,
                        sizeof(struct ubik_value *));

                n_vars = 0;
                for (src_ctor_param = src_ctors->params, j = 0;
                     src_ctor_param != NULL;
                     src_ctor_param = src_ctor_param->next, j++)
                {
                        argtype = src_ctor_param->type_expr;
                        if (argtype->type_expr_type == TYPE_EXPR_VAR)
                        {
                                err = ubik_value_new(&t0, ws);
                                if (err != OK)
                                        return err;
                                t0->type = UBIK_TYP;
                                t0->typ.t = UBIK_TYPE_VAR;
                                t0->typ.var.id = n_vars++;
                                res->typ.adt.ctors[i].arg_types[j] = t0;
                        }
                        else
                        {
                                err = ubik_typesystem_get_from_expr(
                                        &res->typ.adt.ctors[i].arg_types[j],
                                        tsys, argtype, ws);
                                if (err != OK)
                                        goto cleanup;
                        }
                }
        }

        for (src_params = t->adt.params, i = 0;
             src_params != NULL && i < res->typ.adt.n_params;
             src_params = src_params->next, i++)
        {
                res->typ.adt.params[i].data = strdup(src_params->name.name);
                res->typ.adt.params[i].length = strlen(src_params->name.name);
        }

        *res_ptr = res;

        return OK;

cleanup:
        for (j = 0; j <= i; j++)
        {
                free(res->typ.adt.ctors[j].name.data);
                if (res->typ.adt.ctors[j].arg_types != NULL)
                        free(res->typ.adt.ctors[j].arg_types);
        }
        free(res->typ.adt.ctors);
        free(res->typ.adt.params);
        ubik_value_release(res, ws);
        return err;
}

no_ignore ubik_error
ubik_typ_from_ast(
        struct ubik_value **res,
        struct ubik_type *t,
        struct ubik_typesystem *tsys,
        struct ubik_workspace *ws)
{
        switch (t->type)
        {
        case TYPE_RECORD:
        case TYPE_ALIAS:
                return ubik_raise(ERR_NOT_IMPLEMENTED, "records");

        case TYPE_ADT:
                return adt_typ(res, t, tsys, ws);
        }

        return ubik_raise(ERR_BAD_TYPE, "unknown type type");
}
