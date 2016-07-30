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
#include "ubik/ubik.h"
#include "ubik/util.h"
#include "ubik/value.h"

#include <string.h>

static ubik_error
_native_adt_new(struct ubik_env *env, struct ubik_dagc *graph)
{
        struct ubik_value *type_decl;
        struct ubik_value *ctor;
        struct ubik_value *args;
        struct ubik_value *res;
        ubik_error err, cerr;
        size_t i;
        unused(env);

        type_decl = graph->nodes[0]->known.tree;
        ctor = graph->nodes[1]->known.tree;

        err = ubik_value_new(&args);
        if (err != OK)
                return err;
        err = ubik_list_create_empty(args);
        if (err != OK)
                goto free_args;

        for (i = 2; i < graph->n - 1; i++)
        {
                err = ubik_list_append(args, graph->nodes[i]->known.tree);
                if (err != OK)
                        goto free_args;
        }

        err = ubik_value_new(&res);
        if (err != OK)
                return err;
        err = ubik_adt_instantiate(res, type_decl, ctor, args);
        if (err != OK)
                goto free_args;
        graph->result->known.tree = res;

        graph->result->known_type = type_decl;
        err = ubik_take(type_decl);
        if (err != OK)
                goto free_args;

free_args:
        cerr = ubik_release(args);
        if (cerr != OK)
                return err ? err : cerr;

        return err;
}

ubik_error
_register_all_adt_new(struct ubik_env *env)
{
        struct ubik_dagc *graph;
        struct ubik_uri *uri;
        struct ubik_value *type;
        union ubik_value_or_graph ins;
        ubik_error err;
        char *func_name;
        int res;
        int i;

        for (i = 0; i < UBIK_MAX_ADT_FIELDS; i++)
        {
                graph = NULL;
                err = ubik_internal_native_create_op(
                        &graph, i + 2, _native_adt_new);
                if (err != OK)
                        return err;

                res = asprintf(&func_name, "ubik-adt-new-%d", i);
                if (res < 0)
                        return ubik_raise(ERR_NO_MEMORY, "adt new name alloc");
                err = ubik_internal_native_uri(&uri, func_name);
                if (err != OK)
                        return err;
                free(func_name);

                graph->identity = uri;
                err = ubik_take(graph->identity);
                if (err != OK)
                        return err;

                err = ubik_value_new(&type);
                if (err != OK)
                        return err;

                ins.graph = graph;
                err = ubik_env_set(env, uri, ins, type);
                if (err != OK)
                        return err;

                err = ubik_release(type);
                if (err != OK)
                        return err;
                err = ubik_release(graph);
                if (err != OK)
                        return err;
        }

        return OK;
}

no_ignore static ubik_error
_native_adt_ctor_matches(struct ubik_env *env, struct ubik_dagc *graph)
{
        struct ubik_value *inst;
        struct ubik_value *match_name_val;
        char *match_name;
        char *ctor_name;
        size_t match_name_len;
        bool matches;
        ubik_error err;
        unused(env);

        match_name_val = graph->nodes[0]->known.tree;
        inst = graph->nodes[1]->known.tree;
        err = OK;

        err = ubik_string_read(&match_name, &match_name_len, match_name_val);
        if (err != OK)
                return err;

        err = ubik_adt_get_ctor(&ctor_name, inst);
        if (err != OK)
                goto free_match_name;

        matches = strncmp(match_name, ctor_name, match_name_len) == 0;

        err = ubik_value_new(&graph->result->known.tree);
        if (err != OK)
                goto free_ctor_name;
        graph->result->known.tree->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
        graph->result->known.tree->left.w = matches ? 1 : 0;

        err = ubik_value_new(&graph->result->known_type);
        if (err != OK)
                goto free_ctor_name;
        err = ubik_type_bool(graph->result->known_type);
        if (err != OK)
                goto free_ctor_name;

free_ctor_name:
        free(ctor_name);
free_match_name:
        free(match_name);

        return err;
}

ubik_error
_register_adt_ctor_matches(struct ubik_env *env)
{
        struct ubik_dagc *graph;
        struct ubik_uri *uri;
        struct ubik_value *type;
        union ubik_value_or_graph ins;
        ubik_error err;

        graph = NULL;
        err = ubik_internal_native_create_op(
                &graph, 2, _native_adt_ctor_matches);
        if (err != OK)
                return err;

        err = ubik_internal_native_uri(&uri, "ubik-adt-ctor-matches?");
        if (err != OK)
                return err;

        graph->identity = uri;
        err = ubik_take(graph->identity);
        if (err != OK)
                return err;

        err = ubik_value_new(&type);
        if (err != OK)
                return err;

        ins.graph = graph;
        err = ubik_env_set(env, uri, ins, type);
        if (err != OK)
                return err;

        err = ubik_release(type);
        if (err != OK)
                return err;
        err = ubik_release(graph);
        if (err != OK)
                return err;
        return OK;
}

no_ignore static ubik_error
_native_adt_get(struct ubik_env *env, struct ubik_dagc *graph)
{
        struct ubik_value *inst;
        struct ubik_value *index_val;
        struct ubik_value *res;
        ubik_word index;
        ubik_error err, rerr;
        unused(env);

        index_val = graph->nodes[0]->known.tree;
        inst = graph->nodes[1]->known.tree;

        ubik_assert(
                index_val->tag == (TAG_VALUE | TAG_LEFT_WORD | TAG_RIGHT_WORD));
        index = index_val->left.w;

        err = ubik_adt_get_field(&res, inst, index);
        if (err != OK)
                return err;

        graph->result->known.tree = res;
        err = ubik_take(res);
        if (err != OK)
                return err;

        err = ubik_value_new(&graph->result->known_type);
        if (err != OK)
                goto release_res;

        return OK;

release_res:
        rerr = ubik_release(res);
        return err != OK ? err : rerr;
}

ubik_error
_register_adt_get(struct ubik_env *env)
{
        struct ubik_dagc *graph;
        struct ubik_uri *uri;
        struct ubik_value *type;
        union ubik_value_or_graph ins;
        ubik_error err;

        graph = NULL;
        err = ubik_internal_native_create_op(&graph, 2, _native_adt_get);
        if (err != OK)
                return err;

        err = ubik_internal_native_uri(&uri, "ubik-adt-get");
        if (err != OK)
                return err;

        graph->identity = uri;
        err = ubik_take(graph->identity);
        if (err != OK)
                return err;

        err = ubik_value_new(&type);
        if (err != OK)
                return err;

        ins.graph = graph;
        err = ubik_env_set(env, uri, ins, type);
        if (err != OK)
                return err;

        err = ubik_release(type);
        if (err != OK)
                return err;
        err = ubik_release(graph);
        if (err != OK)
                return err;
        return OK;
}
