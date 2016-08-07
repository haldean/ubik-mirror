/*
 * typesystem.c: maintains type information during compiliation
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
#include "ubik/compile.h"
#include "ubik/feedback.h"
#include "ubik/string.h"
#include "ubik/typesystem.h"
#include "ubik/vector.h"

#include <string.h>

enum ts_value_type
{
        TS_TYPE = 1,
        TS_IFACE,
        TS_IMPL,
};

struct ts_type
{
        char *name;
        char *package;
};

struct ts_iface
{
        char *name;
        char *package;
        size_t n_params;
};

struct ts_impl
{
        struct ts_iface *iface;
        struct ubik_type_expr params[UBIK_MAX_INTERFACE_PARAMS];
};

struct ts_value
{
        union
        {
                struct ts_type t;
                struct ts_iface f;
                struct ts_impl m;
        };
        enum ts_value_type vt;
};

struct ubik_typesystem
{
        struct ubik_vector types;
        struct ubik_vector interfaces;
        struct ubik_vector implementations;
        struct ubik_alloc_region *region;
};

no_ignore ubik_error
ubik_typesystem_init(
        struct ubik_typesystem **tsys,
        struct ubik_alloc_region *region)
{
        ubik_alloc1(tsys, struct ubik_typesystem, region);
        (*tsys)->types.region = region;
        (*tsys)->interfaces.region = region;
        (*tsys)->implementations.region = region;
        (*tsys)->region = region;
        return OK;
}

no_ignore ubik_error
ubik_typesystem_load(
        struct ubik_typesystem *tsys,
        struct ubik_ast *ast,
        struct ubik_compile_request *req)
{
        struct ubik_type *t;
        struct ubik_ast_interface *iface;
        struct ubik_ast_implementation *impl;
        struct ubik_type_params *params;
        struct ubik_type_list *type_list;

        struct ts_type *tst;
        struct ts_iface *tsif;
        struct ts_impl *tsim;

        char *check_pkg;
        size_t i;
        size_t j;
        size_t n_params;
        ubik_error err;

        for (i = 0; i < ast->types.n; i++)
        {
                t = (struct ubik_type *) ast->types.elems[i];
                ubik_alloc1(&tst, struct ts_type, tsys->region);
                tst->name = ubik_strdup(t->name, tsys->region);
                tst->package = ubik_strdup(ast->package_name, tsys->region);
                err = ubik_vector_append(&tsys->types, tst);
                if (err != OK)
                        return err;
        }

        for (i = 0; i < ast->interfaces.n; i++)
        {
                iface = (struct ubik_ast_interface *) ast->interfaces.elems[i];
                ubik_alloc1(&tsif, struct ts_iface, tsys->region);
                tsif->name = ubik_strdup(iface->name, tsys->region);
                tsif->package = ubik_strdup(ast->package_name, tsys->region);
                for (params = iface->params, tsif->n_params = 0;
                        params != NULL;
                        params = params->next, tsif->n_params++);

                if (tsif->n_params > UBIK_MAX_INTERFACE_PARAMS)
                {
                        ubik_feedback_error_line(
                                req->feedback,
                                UBIK_FEEDBACK_ERR,
                                &iface->loc,
                                "interfaces cannot have more than %d "
                                "parameters, %s has %lu",
                                UBIK_MAX_INTERFACE_PARAMS,
                                iface->name,
                                tsif->n_params);
                        return ubik_raise(
                                ERR_UNKNOWN_TYPE, "too many interface params");
                }

                err = ubik_vector_append(&tsys->interfaces, tsif);
                if (err != OK)
                        return err;
        }

        for (i = 0; i < ast->implementations.n; i++)
        {
                impl = (struct ubik_ast_implementation *)
                        ast->implementations.elems[i];
                ubik_alloc1(&tsim, struct ts_impl, tsys->region);
                tsim->iface = NULL;
                check_pkg = impl->iface_package == NULL ?
                        ast->package_name : impl->iface_package;

                for (j = 0; j < tsys->interfaces.n; j++)
                {
                        tsif = tsys->interfaces.elems[j];
                        if (ubik_strcmp(tsif->name, impl->iface_name) != 0)
                                continue;
                        if (ubik_strcmp(tsif->package, check_pkg) != 0)
                                continue;
                        tsim->iface = tsif;
                        break;
                }
                if (tsim->iface == NULL && impl->iface_package == NULL)
                {
                        ubik_feedback_error_line(
                                req->feedback,
                                UBIK_FEEDBACK_ERR,
                                &impl->loc,
                                "implementation of unknown interface %s",
                                impl->iface_name);
                        return ubik_raise(
                                ERR_UNKNOWN_TYPE, "impl of unknown interface");
                }

                for (n_params = 0, type_list = impl->params;
                        type_list != NULL;
                        type_list = type_list->next, n_params++);
                if (n_params != tsim->iface->n_params)
                {
                        ubik_feedback_error_line(
                                req->feedback,
                                UBIK_FEEDBACK_ERR,
                                &impl->loc,
                                "too many interface parameters given, "
                                "interface %s has %lu parameters, but "
                                "implementation has %lu",
                                tsim->iface->name,
                                tsim->iface->n_params,
                                n_params);
                        return ubik_raise(
                                ERR_UNKNOWN_TYPE, "impl of unknown interface");
                }

                for (j = 0, type_list = impl->params;
                        type_list != NULL;
                        type_list = type_list->next, j++)
                {
                        err = ubik_type_expr_copy(
                                &tsim->params[j],
                                type_list->type_expr,
                                tsys->region);
                        if (err != OK)
                                return err;
                }

                err = ubik_vector_append(&tsys->implementations, tsim);
                if (err != OK)
                        return err;
        }

        return OK;
}

void
ubik_typesystem_dump(struct ubik_typesystem *tsys)
{
        size_t i;
        size_t j;
        struct ts_type *type;
        struct ts_iface *iface;
        struct ts_impl *impl;

        printf("\n%lu known types:\n", tsys->types.n);
        for (i = 0; i < tsys->types.n; i++)
        {
                type = tsys->types.elems[i];
                printf("\t%s:%s\n", type->package, type->name);
        }
        printf("%lu known interfaces:\n", tsys->interfaces.n);
        for (i = 0; i < tsys->interfaces.n; i++)
        {
                iface = tsys->interfaces.elems[i];
                printf("\t%s:%s %lu\n",
                        iface->package, iface->name, iface->n_params);
        }
        printf("%lu known implementations:\n", tsys->implementations.n);
        for (i = 0; i < tsys->implementations.n; i++)
        {
                impl = tsys->implementations.elems[i];
                printf("\t%s:%s", impl->iface->package, impl->iface->name);
                for (j = 0; j < impl->iface->n_params; j++)
                {
                        printf(" ");
                        ubik_assert(
                                ubik_type_expr_print(&impl->params[j]) == OK);
                }
                printf("\n");
        }
}

no_ignore ubik_error
ubik_typesystem_unify(
        struct ubik_type_expr **unified,
        struct ubik_typesystem *tsys,
        char *package,
        struct ubik_type_expr *assign_to,
        struct ubik_type_expr *assign_from)
{
        struct ts_value to;
        struct ts_value from;

        find(&to, tsys, package, assign_to);
        find(&from, tsys, package, assign_from);
}
