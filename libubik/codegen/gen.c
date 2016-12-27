/*
 * gen.c: ubik bytecode generation
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

#include "ubik/adt.h"
#include "ubik/alloc.h"
#include "ubik/assert.h"
#include "ubik/assign.h"
#include "ubik/env.h"
#include "ubik/fun.h"
#include "ubik/gen.h"
#include "ubik/resolve.h"
#include "ubik/rttypes.h"
#include "ubik/string.h"
#include "ubik/ubik.h"
#include "ubik/uri.h"
#include "ubik/util.h"
#include "ubik/value.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

no_ignore static ubik_error
ubik_compile_binding(
        struct ubik_ast_binding *binding,
        struct ubik_env *local_env,
        struct ubik_assign_context *ctx)
{
        struct ubik_uri *uri;
        struct ubik_value *res;
        struct ubik_value *type;
        struct ubik_vector nodes = {.region = ctx->region};

        ubik_error err;

        err = ubik_assign_nodes(ctx, &nodes, binding->expr);
        if (err != OK)
                return err;

        err = ubik_value_new(&res, ctx->workspace);
        if (err != OK)
                return err;
        ubik_fun_from_vector(res, &nodes, binding->expr->gen);

        res->dbg = binding->expr->dbginfo;
        if (binding->expr->dbginfo.name != NULL)
                res->dbg.name = ubik_strdup(binding->expr->dbginfo.name, NULL);

        /* TODO: add binding type here */
        err = ubik_value_new(&type, ctx->workspace);
        if (err != OK)
                return err;

        ubik_galloc1(&uri, struct ubik_uri);
        err = ubik_uri_package(
                uri, binding->expr->scope->package_name, binding->name);
        if (err != OK)
                return err;

        err = ubik_env_set(local_env, uri, res, type);
        ubik_uri_free(uri);
        if (err != OK)
                return err;

        return OK;
}

struct modinit_iterator
{
        struct ubik_vector *nodes;
        char *package_name;
        struct ubik_alloc_region *region;
};

no_ignore static ubik_error
_add_modinit_setter(
        void *viter,
        struct ubik_env *env,
        struct ubik_uri *uri)
{
        struct ubik_value *value;
        struct ubik_value *type;
        struct ubik_node *store_node;
        struct ubik_node *val_node;
        struct modinit_iterator *iter;
        struct ubik_uri *store_uri;
        ubik_error err;

        iter = (struct modinit_iterator *) viter;

        err = ubik_env_get(&value, &type, env, uri);
        if (err != OK)
                return err;

        ubik_alloc1(&store_uri, struct ubik_uri, iter->region);
        err = ubik_uri(
                store_uri,
                iter->package_name, strlen(iter->package_name),
                uri->name, uri->name_len);
        if (err != OK)
                return err;

        ubik_alloc1(&val_node, struct ubik_node, iter->region);
        val_node->node_type = UBIK_VALUE;
        val_node->id = iter->nodes->n;
        val_node->value.type = type;
        val_node->value.value = value;

        ubik_alloc1(&store_node, struct ubik_node, iter->region);
        store_node->node_type = UBIK_STORE;
        store_node->id = iter->nodes->n + 1;
        store_node->is_terminal = true;
        store_node->store.loc = ubik_uri_dup(store_uri);
        store_node->store.value = val_node->id;

        err = ubik_vector_append(iter->nodes, val_node);
        if (err != OK)
                return err;
        err = ubik_vector_append(iter->nodes, store_node);
        if (err != OK)
                return err;

        return OK;
}

no_ignore static ubik_error
ubik_create_modinit(
        struct ubik_ast *ast,
        struct ubik_env *local_env,
        enum ubik_load_reason load_reason,
        struct ubik_assign_context *ctx)
{
        struct modinit_iterator iter;
        struct ubik_vector nodes = {.region = ctx->region};
        struct ubik_vector imm_nodes = {.region = ctx->region};
        struct ubik_value *imm_value;
        struct ubik_value *modinit;
        ubik_word result;
        ubik_error err;

        iter.nodes = &nodes;
        iter.package_name = ast->package_name;
        iter.region = ctx->region;

        err = ubik_env_iterate(_add_modinit_setter, local_env, &iter);
        if (err != OK)
                return err;

        if (nodes.n > 0)
        {
                /* All graphs have to have a result, so we just pick one here.
                 * We'll end up executing all of them anyway, and nothing reads
                 * the modinit's result. */
                result = nodes.n - 1;

                err = ubik_value_new(&modinit, ctx->workspace);
                if (err != OK)
                        return err;

                ubik_fun_from_vector(modinit, &nodes, result);
                modinit->gc.root = true;
                modinit->gc.modinit = true;
        }

        if (ast->immediate != NULL
                && (load_reason == LOAD_MAIN || load_reason == LOAD_BLOCK))
        {
                err = ubik_assign_nodes(ctx, &imm_nodes, ast->immediate);
                if (err != OK)
                        return err;

                err = ubik_value_new(&imm_value, ctx->workspace);
                if (err != OK)
                        return err;

                ubik_fun_from_vector(
                        imm_value, &imm_nodes, ast->immediate->gen);
                imm_value->gc.root = true;
                imm_value->gc.modinit = false;
        }

        return OK;
}

no_ignore ubik_error
ubik_gen_graphs(
        struct ubik_ast *ast,
        struct ubik_compile_request *req)
{
        size_t i;
        ubik_error err, rerr;
        struct ubik_env local_env;
        local(assign_context) struct ubik_assign_context ctx = {0};

        ctx.region = &req->region;
        ctx.feedback = req->feedback;
        ctx.errors.region = &req->region;
        ctx.workspace = req->workspace;

        err = ubik_env_init(&local_env);
        if (err != OK)
                return err;

        for (i = 0; i < ast->bindings.n; i++)
        {
                err = ubik_compile_binding(
                        ast->bindings.elems[i], &local_env, &ctx);
                if (err != OK)
                        goto cleanup_env;
        }

        err = ubik_create_modinit(ast, &local_env, req->reason, &ctx);
        if (err != OK)
        {
                if (err->error_code == ERR_NO_DATA)
                        printf("source has no data in it\n");
                goto cleanup_env;
        }

        if (ubik_assign_emit_errors(&ctx))
        {
                err = ubik_raise(
                        ERR_BAD_VALUE,
                        "node assignment failed.");
                goto cleanup_env;
        }

cleanup_env:
        rerr = ubik_env_free(&local_env);
        return err == OK ? rerr : err;
}
