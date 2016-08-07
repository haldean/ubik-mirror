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

#include "ubik/compile.h"
#include "ubik/feedback.h"
#include "ubik/string.h"
#include "ubik/typesystem.h"
#include "ubik/vector.h"

#include <string.h>

struct ts_type
{
        char *name;
        char *package;
};

struct ts_iface
{
        char *name;
        char *package;
        int n_params;
};

struct ts_impl
{
        struct ts_iface *iface;
        char *params[UBIK_MAX_INTERFACE_PARAMS];
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
        size_t i;
        size_t j;
        struct ubik_type *t;
        struct ubik_ast_interface *iface;
        struct ubik_ast_implementation *impl;
        struct ts_type *tst;
        struct ts_iface *tsif;
        struct ts_impl *tsim;
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
                ubik_alloc1(&tsif, struct ts_type, tsys->region);
                tsif->name = ubik_strdup(iface->name, tsys->region);
                tsif->package = ubik_strdup(ast->package_name, tsys->region);
                err = ubik_vector_append(&tsys->interfaces, tsif);
                if (err != OK)
                        return err;
        }

        for (i = 0; i < ast->implementations.n; i++)
        {
                impl = (struct ubik_ast_implementation *)
                        ast->implementations.elems[i];
                ubik_alloc1(&tsim, struct ts_type, tsys->region);
                tsim->iface = NULL;

                for (j = 0; j < tsys->interfaces.n; j++)
                {
                        tsif = tsys->interfaces.elems[j];
                        if (ubik_strcmp(tsif->name, impl->iface_name) != 0)
                                continue;
                        if (ubik_strcmp(tsif->package, impl->iface_package) != 0)
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

                err = ubik_vector_append(&tsys->implementations, tsif);
                if (err != OK)
                        return err;
        }

        return OK;
}
