/*
 * clash.c: make sure names don't clash
 * Copyright (C) 2017, Haldean Brown
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
#include "ubik/feedback.h"
#include "ubik/stringset.h"
#include "ubik/ubik.h"
#include "ubik/walk.h"

#include <string.h>

static void
print_dupe(
        struct ubik_stream *feedback,
        struct ubik_ast *ast,
        struct ubik_ast_binding *bind,
        size_t i)
{
        size_t j;
        struct ubik_ast_binding *other;

        other = NULL;
        for (j = 0; j < i; j++)
        {
                other = ast->bindings.elems[j];
                if (strcmp(other->name, bind->name) == 0)
                        break;
        }
        ubik_assert(other != NULL);

        ubik_feedback_line(
                feedback,
                UBIK_FEEDBACK_ERR,
                &other->loc,
                "tried to assign to \x1b[32m%s\x1b[0m more than once",
                bind->name);
        ubik_feedback_line(
                feedback,
                UBIK_FEEDBACK_ERR,
                &bind->loc,
                "...previous definition was here");
}

struct clash_info
{
        struct ubik_walk_info head;
        struct ubik_stream *feedback;
};

no_ignore static ubik_error
visit_ast(struct ubik_walk_info *wi, struct ubik_ast *ast)
{
        struct ubik_ast_binding *bind;
        struct clash_info *ci;
        local(stringset) struct ubik_stringset s = {0};
        size_t i;
        ubik_error err;

        ci = (struct clash_info *) wi;

        for (i = 0; i < ast->bindings.n; i++)
        {
                bind = ast->bindings.elems[i];
                if (ubik_stringset_present(&s, bind->name))
                {
                        print_dupe(ci->feedback, ast, bind, i);
                        return ubik_raise(
                                ERR_PRESENT, "name was bound twice");
                }
                err = ubik_stringset_add(&s, bind->name);
                if (err != OK)
                        return err;
        }
        return OK;
}

no_ignore ubik_error
ubik_clash_check(struct ubik_ast *ast, struct ubik_stream *feedback)
{
        struct clash_info ci = {
                .head = {
                        .ast = visit_ast,
                },
                .feedback = feedback,
        };
        return ubik_walk_ast(ast, &ci.head);
}
