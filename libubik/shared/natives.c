/*
 * natives.c: built-in native methods
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

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include "ubik/adt.h"
#include "ubik/assert.h"
#include "ubik/dagc.h"
#include "ubik/env.h"
#include "ubik/list.h"
#include "ubik/natives.h"
#include "ubik/types.h"
#include "ubik/ubik.h"
#include "ubik/util.h"
#include "ubik/value.h"

#include <inttypes.h>
#include <string.h>

extern ubik_error _register_adt_ctor_matches(struct ubik_env *env);
extern ubik_error _register_adt_get(struct ubik_env *env);
extern ubik_error _register_all_adt_new(struct ubik_env *env);
extern ubik_error _register_uadd(struct ubik_env *env);
extern ubik_error _register_usub(struct ubik_env *env);
extern ubik_error _register_eq(struct ubik_env *env);
extern ubik_error _register_emit(struct ubik_env *env);
extern ubik_error _register_humanize(struct ubik_env *env);
extern ubik_error _register_concat(struct ubik_env *env);

no_ignore ubik_error
ubik_internal_native_uri(struct ubik_uri **uri, char *name)
{
        ubik_error err;
        size_t name_len;

        *uri = calloc(1, sizeof(struct ubik_uri));
        if (*uri == NULL)
                return ubik_raise(ERR_NO_MEMORY, "create native uri");

        name_len = strlen(name);
        if (unlikely(name_len < 1))
                return ubik_raise(ERR_BAD_VALUE, "native uri must have name");

        err = ubik_uri_native(*uri, name);
        return err;
}

no_ignore ubik_error
ubik_internal_native_create_op(
        struct ubik_dagc **graph_ptr,
        size_t arity,
        ubik_native_evaluator_t evaluator)
{
        struct ubik_dagc *graph;
        struct ubik_dagc_native *ngraph;
        struct ubik_dagc_input *in;
        size_t i;
        ubik_error err;

        err = ubik_dagc_alloc(
                &graph, arity + 1, sizeof(struct ubik_dagc_native), NULL);
        if (err != OK)
                return err;
        ngraph = (struct ubik_dagc_native *) graph;
        *graph_ptr = graph;

        /* Create input nodes */
        for (i = 0; i < arity; i++)
        {
                in = (struct ubik_dagc_input *) graph->nodes[i];
                if (in == NULL)
                        return ubik_raise(ERR_NO_MEMORY, "create native dagc");
                in->head.node_type = DAGC_NODE_INPUT;
                in->head.known_type = NULL;
                in->head.known.any = NULL;
                in->head.is_terminal = 0x00;
                in->head.flags = 0x00;
                in->arg_num = i;
        }

        /* Create output native node */
        graph->nodes[arity]->node_type = DAGC_NODE_NATIVE;
        graph->nodes[arity]->known_type = NULL;
        graph->nodes[arity]->known.any = NULL;
        graph->nodes[arity]->is_terminal = 0x01;
        graph->nodes[arity]->flags = 0x00;

        graph->result = graph->nodes[arity];

        err = ubik_dagc_init(graph);
        if (err != OK)
                return err;
        graph->tag |= TAG_GRAPH_NATIVE;
        ngraph->evaluator = evaluator;

        return OK;
}

struct native_record
{
        char *name;
        char *type_string;
        void *type_record;
};

static struct native_record native_funcs[] = {
        { "uadd",                   "Word -> Word -> Word",           NULL },
        { "usub",                   "Word -> Word -> Word",           NULL },
        { "eq",                     "a -> a -> Boolean | Eq a",       NULL },
        { "emit",                   "String -> Unit",                 NULL },
        { "humanize",               "a -> String | Humanize a",       NULL },
        { "concat",                 "String -> String -> String",     NULL },
        { "ubik-adt-ctor-matches?", NULL,                             NULL },
        { "ubik-adt-get",           NULL,                             NULL },
        { "ubik-adt-new-0",         NULL,                             NULL },
        { "ubik-adt-new-1",         NULL,                             NULL },
        { "ubik-adt-new-2",         NULL,                             NULL },
        { "ubik-adt-new-3",         NULL,                             NULL },
        { "ubik-adt-new-4",         NULL,                             NULL },
        { "ubik-adt-new-5",         NULL,                             NULL },
        { "ubik-adt-new-6",         NULL,                             NULL },
        { "ubik-adt-new-7",         NULL,                             NULL },
        { "ubik-adt-new-8",         NULL,                             NULL },
        { "ubik-adt-new-9",         NULL,                             NULL },
        { "ubik-adt-new-10",        NULL,                             NULL },
        { "ubik-adt-new-11",        NULL,                             NULL },
        { "ubik-adt-new-12",        NULL,                             NULL },
        { "ubik-adt-new-13",        NULL,                             NULL },
        { "ubik-adt-new-14",        NULL,                             NULL },
        { "ubik-adt-new-15",        NULL,                             NULL },
        { "ubik-adt-new-16",        NULL,                             NULL },
        { "ubik-adt-new-17",        NULL,                             NULL },
        { "ubik-adt-new-18",        NULL,                             NULL },
        { "ubik-adt-new-19",        NULL,                             NULL },
        { "ubik-adt-new-20",        NULL,                             NULL },
        { "ubik-adt-new-21",        NULL,                             NULL },
        { "ubik-adt-new-22",        NULL,                             NULL },
        { "ubik-adt-new-23",        NULL,                             NULL },
        { "ubik-adt-new-24",        NULL,                             NULL },
        { "ubik-adt-new-25",        NULL,                             NULL },
        { "ubik-adt-new-26",        NULL,                             NULL },
        { "ubik-adt-new-27",        NULL,                             NULL },
        { "ubik-adt-new-28",        NULL,                             NULL },
        { "ubik-adt-new-29",        NULL,                             NULL },
        { "ubik-adt-new-30",        NULL,                             NULL },
        { "ubik-adt-new-31",        NULL,                             NULL },
        { "ubik-adt-new-32",        NULL,                             NULL }
#if UBIK_MAX_ADT_FIELDS != 32
#error "the list of native funcs needs to be updated"
#endif
};

no_ignore ubik_error
ubik_natives_register(struct ubik_env *env)
{
        ubik_error err;

        err = _register_uadd(env);
        if (err != OK)
                return err;

        err = _register_usub(env);
        if (err != OK)
                return err;

        err = _register_eq(env);
        if (err != OK)
                return err;

        err = _register_emit(env);
        if (err != OK)
                return err;

        err = _register_humanize(env);
        if (err != OK)
                return err;

        err = _register_concat(env);
        if (err != OK)
                return err;

        err = _register_all_adt_new(env);
        if (err != OK)
                return err;

        err = _register_adt_ctor_matches(env);
        if (err != OK)
                return err;

        err = _register_adt_get(env);
        if (err != OK)
                return err;

        return OK;
}

bool
ubik_natives_is_defined(char *name)
{
        size_t i;
        size_t n;
        n = sizeof(native_funcs) / sizeof(native_funcs[0]);
        for (i = 0; i < n; i++)
                if (strcmp(native_funcs[i].name, name) == 0)
                        return true;
        return false;
}
