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
#include <string.h>
#include <stdlib.h>

no_ignore ubik_error
add_splat(
        struct ubik_compile_env *cenv,
        struct ubik_ast *ast,
        char *canonical)
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
                new_bind = calloc(1, sizeof(struct ubik_ast_binding));
                if (new_bind == NULL)
                        return ubik_raise(ERR_NO_MEMORY, "splat binding alloc");
                new_bind->name = strdup(old_bind->name);
                new_bind->expr = calloc(1, sizeof(struct ubik_ast_expr));
                if (new_bind->expr == NULL)
                {
                        free(new_bind);
                        return ubik_raise(ERR_NO_MEMORY, "splat binding alloc");
                }
                new_bind->expr->expr_type = EXPR_ATOM;
                new_bind->expr->atom = calloc(1, sizeof(struct ubik_ast_atom));
                if (new_bind->expr->atom == NULL)
                {
                        free(new_bind->expr);
                        free(new_bind);
                        return ubik_raise(ERR_NO_MEMORY, "splat binding alloc");
                }
                new_bind->expr->atom->atom_type = ATOM_QUALIFIED;
                new_bind->expr->atom->qualified.head = strdup(canonical);
                new_bind->expr->atom->qualified.tail = strdup(old_bind->name);
                new_bind->loc = old_bind->loc;

                new_bind->type_expr = calloc(
                        1, sizeof(struct ubik_ast_type_expr));
                if (new_bind->type_expr == NULL)
                {
                        free(new_bind->expr->atom);
                        free(new_bind->expr);
                        free(new_bind);
                        return ubik_raise(ERR_NO_MEMORY, "splat binding alloc");
                }
                err = ubik_ast_type_expr_copy(
                        new_bind->type_expr, old_bind->type_expr);
                if (err != OK)
                        return err;

                err = ubik_vector_append(&ast->bindings, new_bind);
                if (err != OK)
                        return err;
        }

        /* TODO: types */

        return OK;
}

no_ignore ubik_error
ubik_import_add_splats(
        struct ubik_compile_env *cenv,
        struct ubik_ast *ast)
{
        struct ubik_ast_import_list *import;
        ubik_error err;

        import = ast->imports;
        while (import != NULL)
        {
                if (strcmp(import->name, "") == 0)
                {
                        err = add_splat(cenv, ast, import->canonical);
                        if (err != OK)
                                return err;
                }
                import = import->next;
        }
        return OK;
}

