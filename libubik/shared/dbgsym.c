/*
 * dbgsym.h: debug symbol support
 * Copyright (C) 2015, Haldean Brown
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

#include <string.h>

#include "ubik/assert.h"
#include "ubik/ast.h"
#include "ubik/dbgsym.h"
#include "ubik/rt.h"
#include "ubik/string.h"

void
ubik_dbgsym_mark_trace(struct ubik_workspace *ws, char *name)
{
        size_t i;
        struct ubik_value *v;
        bool all;

        all = strcmp(name, "*") == 0;

        while (ws != NULL)
        {
                for (i = 0; i < ws->n; i++)
                {
                        v = &ws->values[i];
                        if (!all)
                        {
                                if (!v->dbg.used)
                                        continue;
                                if (strcmp(v->dbg.name, name))
                                        continue;
                        }
                        v->gc.traced = true;
                }
                ws = ws->next;
        }
}

static void
ubik_dbgsym_attach_expr(struct ubik_ast_expr *expr, struct ubik_alloc_region *r)
{
        struct ubik_ast *subast;
        struct ubik_ast_expr *subexprs[UBIK_MAX_SUBEXPRS];
        size_t n;
        size_t i;

        ubik_assert(ubik_ast_subexprs(&subast, subexprs, &n, expr) == OK);
        for (i = 0; i < n; i++)
                ubik_dbgsym_attach_expr(subexprs[i], r);
        if (subast != NULL)
                ubik_dbgsym_attach(subast, r);
}

void
ubik_dbgsym_attach(struct ubik_ast *ast, struct ubik_alloc_region *r)
{
        struct ubik_ast_binding *bind;
        char *name;
        size_t i;

        for (i = 0; i < ast->bindings.n; i++)
        {
                bind = (struct ubik_ast_binding *) ast->bindings.elems[i];
                bind->expr->dbginfo.used = true;
                bind->expr->dbginfo.line = bind->loc.line_start;
                bind->expr->dbginfo.col = bind->loc.col_start;

                ubik_ralloc(
                        (void **) &name,
                        strlen(bind->name) + strlen(ast->package_name) + 2,
                        sizeof(char), r);
                strcat(name, ast->package_name);
                name[strlen(ast->package_name)] = ':';
                strcat(name, bind->name);
                bind->expr->dbginfo.name = name;

                ubik_dbgsym_attach_expr(bind->expr, r);
        }

        if (ast->immediate != NULL)
                ubik_dbgsym_attach_expr(ast->immediate, r);
}
