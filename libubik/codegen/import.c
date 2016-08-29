/*
 * import.c: packaging and importing utilities
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

#include "ubik/import.h"
#include "ubik/string.h"
#include "ubik/types.h"

#include <string.h>
#include <stdlib.h>

no_ignore ubik_error
add_splat(
        struct ubik_compile_env *cenv,
        struct ubik_ast *ast,
        char *canonical,
        struct ubik_alloc_region *region)
{
        struct ubik_compile_result *cres;
        struct ubik_ast_binding *new_bind;
        struct ubik_ast_binding *old_bind;
        bool found;
        size_t i;
        ubik_error err;

        found = false;
        for (i = 0; i < cenv->compiled.n; i++)
        {
                cres = cenv->compiled.elems[i];
                if (strcmp(cres->ast->package_name, canonical) == 0)
                {
                        found = true;
                        break;
                }
        }
        if (!found)
                return ubik_raise(ERR_ABSENT, "splat import not compiled");

        for (i = 0; i < cres->ast->bindings.n; i++)
        {
                old_bind = cres->ast->bindings.elems[i];

                ubik_alloc1(&new_bind, struct ubik_ast_binding, region);
                new_bind->name = ubik_strdup(old_bind->name, region);
                new_bind->loc = old_bind->loc;

                ubik_alloc1(&new_bind->expr, struct ubik_ast_expr, region);
                new_bind->expr->expr_type = EXPR_ATOM;
                new_bind->expr->loc = old_bind->loc;

                ubik_alloc1(&new_bind->expr->atom, struct ubik_ast_atom, region);
                new_bind->expr->atom->atom_type = ATOM_QUALIFIED;
                new_bind->expr->atom->qualified.head =
                        ubik_strdup(canonical, region);
                new_bind->expr->atom->qualified.tail =
                        ubik_strdup(old_bind->name, region);
                new_bind->expr->atom->loc = old_bind->loc;

                if (old_bind->type_expr != NULL)
                {
                        ubik_alloc1(
                                &new_bind->type_expr,
                                struct ubik_type_expr,
                                region);
                        err = ubik_type_expr_copy(
                                new_bind->type_expr, old_bind->type_expr,
                                region);
                        if (err != OK)
                                return err;
                }
                else
                        new_bind->type_expr = NULL;

                err = ubik_vector_append(&ast->bindings, new_bind);
                if (err != OK)
                        return err;
        }

        return OK;
}

no_ignore ubik_error
ubik_import_add_splats(
        struct ubik_compile_env *cenv,
        struct ubik_ast *ast,
        struct ubik_alloc_region *region)
{
        struct ubik_ast_import_list *import;
        ubik_error err;

        import = ast->imports;
        while (import != NULL)
        {
                if (strcmp(import->name, "") == 0)
                {
                        err = add_splat(cenv, ast, import->canonical, region);
                        if (err != OK)
                                return err;
                }
                import = import->next;
        }
        return OK;
}

