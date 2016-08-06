/*
 * interfaces.c: interface and implementation code generation
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

#include "ubik/alloc.h"
#include "ubik/interfaces.h"
#include "ubik/string.h"
#include "ubik/types.h"
#include "ubik/ubik.h"
#include "ubik/uri.h"

no_ignore static ubik_error
add_multimethod(
        struct ubik_ast *ast,
        struct ubik_ast_member_list *member,
        struct ubik_compile_request *req)
{
        struct ubik_ast_expr *e0, *e1, *e2;
        struct ubik_type_expr *t0;
        struct ubik_ast_binding *bind;
        struct ubik_uri uri;
        struct ubik_ast_arg_list *args;
        ubik_error err;
        int n_args;
        int i;

        for (n_args = 0, t0 = member->type;
                t0->type_expr_type == TYPE_EXPR_ARROW;
                n_args++, t0 = t0->apply.tail);

        err = ubik_uri_nocopy(&uri, ast->package_name, member->name);
        if (err != OK)
                return err;
        err = ubik_uri_attach_value(&uri);
        if (err != OK)
                return err;

        ubik_alloc1(&e0, struct ubik_ast_expr, &req->region);
        e0->expr_type = EXPR_ATOM;
        e0->loc = member->loc;
        ubik_alloc1(&e0->atom, struct ubik_ast_atom, &req->region);
        e0->atom->atom_type = ATOM_NAME;
        e0->atom->loc = e0->loc;
        ubik_asprintf(
                &e0->atom->str, &req->region,
                "ubik-multimethod-call-%d", n_args);

        ubik_alloc1(&e1, struct ubik_ast_expr, &req->region);
        e1->expr_type = EXPR_ATOM;
        e1->loc = member->loc;
        ubik_alloc1(&e1->atom, struct ubik_ast_atom, &req->region);
        e1->atom->atom_type = ATOM_VALUE;
        e1->atom->loc = e1->loc;
        e1->atom->value = uri.as_value;
        ubik_ref_steal(e1->atom->value, &req->region);

        ubik_alloc1(&e2, struct ubik_ast_expr, &req->region);
        e2->expr_type = EXPR_APPLY;
        e2->loc = member->loc;
        e2->apply.head = e0;
        e2->apply.tail = e1;

        for (i = 0; i < n_args; i++)
        {
                ubik_alloc1(&e0, struct ubik_ast_expr, &req->region);
                e0->expr_type = EXPR_ATOM;
                e0->loc = member->loc;
                ubik_alloc1(&e0->atom, struct ubik_ast_atom, &req->region);
                e0->atom->atom_type = ATOM_NAME;
                e0->atom->loc = e0->loc;
                ubik_asprintf(&e0->atom->str, &req->region, "%d", i);

                e1 = e2;

                ubik_alloc1(&e2, struct ubik_ast_expr, &req->region);
                e2->expr_type = EXPR_APPLY;
                e2->loc = member->loc;
                e2->apply.head = e1;
                e2->apply.tail = e0;
        }

        ubik_alloc1(&e0, struct ubik_ast_expr, &req->region);
        e0->expr_type = EXPR_LAMBDA;
        e0->loc = member->loc;
        e0->lambda.args = NULL;
        for (i = n_args - 1; i >= 0; i--)
        {
                ubik_alloc1(&args, struct ubik_ast_arg_list, &req->region);
                ubik_asprintf(&args->name, &req->region, "%d", i);
                args->next = e0->lambda.args;
                e0->lambda.args = args;
        }
        e0->lambda.body = e2;

        ubik_alloc1(&bind, struct ubik_ast_binding, &req->region);
        bind->name = ubik_strdup(member->name, &req->region);
        bind->expr = e0;
        return ubik_vector_append(&ast->bindings, bind);
}

no_ignore static ubik_error
compile_interface(
        struct ubik_ast *ast,
        struct ubik_ast_interface *iface,
        struct ubik_compile_request *req)
{
        struct ubik_ast_member_list *member;
        ubik_error err;

        for (member = iface->members; member != NULL; member = member->next)
        {
                err = add_multimethod(ast, member, req);
                if (err != OK)
                        return err;
        }

        return OK;
}

no_ignore ubik_error
ubik_interfaces_compile_all(
        struct ubik_ast *ast,
        struct ubik_compile_request *req)
{
        struct ubik_ast_interface *iface;
        ubik_error err;
        size_t i;

        for (i = 0; i < ast->interfaces.n; i++)
        {
                iface = (struct ubik_ast_interface *) ast->interfaces.elems[i];
                err = compile_interface(ast, iface, req);
                if (err != OK)
                        return err;
        }

        return OK;
}
