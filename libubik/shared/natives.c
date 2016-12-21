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
#include "ubik/env.h"
#include "ubik/natives.h"
#include "ubik/parse.h"
#include "ubik/types.h"
#include "ubik/ubik.h"
#include "ubik/util.h"

#include <dlfcn.h>
#include <string.h>

extern ubik_error _register_adt_ctor_matches(
        struct ubik_env *env, struct ubik_workspace *ws);
extern ubik_error _register_adt_get(
        struct ubik_env *env, struct ubik_workspace *ws);
extern ubik_error _register_all_adt_new(
        struct ubik_env *env, struct ubik_workspace *ws);
extern ubik_error _register_rational_add(
        struct ubik_env *env, struct ubik_workspace *ws);
extern ubik_error _register_rational_subtract(
        struct ubik_env *env, struct ubik_workspace *ws);
extern ubik_error _register_rational_multiply(
        struct ubik_env *env, struct ubik_workspace *ws);
extern ubik_error _register_rational_divide(
        struct ubik_env *env, struct ubik_workspace *ws);
extern ubik_error _register_rational_remainder(
        struct ubik_env *env, struct ubik_workspace *ws);
extern ubik_error _register_eq(
        struct ubik_env *env, struct ubik_workspace *ws);
extern ubik_error _register_emit(
        struct ubik_env *env, struct ubik_workspace *ws);
extern ubik_error _register_humanize(
        struct ubik_env *env, struct ubik_workspace *ws);
extern ubik_error _register_concat(
        struct ubik_env *env, struct ubik_workspace *ws);
extern ubik_error _register_boolean_true(
        struct ubik_env *env, struct ubik_workspace *ws);
extern ubik_error _register_boolean_false(
        struct ubik_env *env, struct ubik_workspace *ws);

no_ignore ubik_error
ubik_internal_native_uri(struct ubik_uri **uri, char *name)
{
        ubik_error err;
        size_t name_len;

        *uri = calloc(1, sizeof(struct ubik_uri));
        if (*uri == NULL)
                return ubik_raise(ERR_NO_MEMORY, "create native uri");

        name_len = strlen(name);
        if (unlikely(name_len == 0))
                return ubik_raise(ERR_BAD_VALUE, "native uri must have name");

        err = ubik_uri_native(*uri, name);
        return err;
}

no_ignore ubik_error
ubik_internal_native_create_op(
        struct ubik_value **graph_ptr,
        size_t arity,
        ubik_graph_evaluator_t evaluator,
        struct ubik_workspace *ws)
{
        struct ubik_value *graph;
        struct ubik_node *in;
        size_t i;
        ubik_error err;

        err = ubik_value_new(&graph, ws);
        if (err != OK)
                return err;
        graph->gc.runtime_managed = true;

        *graph_ptr = graph;

        graph->type = UBIK_FUN;
        graph->fun.n = arity + 1;
        graph->fun.arity = arity;
        graph->fun.result = arity;
        graph->fun.evaluator = evaluator;

        ubik_galloc((void**) &graph->fun.nodes,
                    graph->fun.n, sizeof(struct ubik_node));

        /* Create input nodes */
        for (i = 0; i < arity; i++)
        {
                in = &graph->fun.nodes[i];
                in->node_type = UBIK_INPUT;
                in->id = i;
                in->is_terminal = false;
                in->input.arg_num = i;
        }

        /* Create output native node */
        graph->fun.nodes[arity].node_type = UBIK_NATIVE;
        graph->fun.nodes[arity].id = arity;
        graph->fun.nodes[arity].is_terminal = true;

        return OK;
}

no_ignore ubik_error
ubik_natives_load_hook(char *path)
{
        void *dl;
        ubik_hook_installer inst;

        dl = dlopen(path, RTLD_NOW);
        if (dl == NULL)
        {
                printf("could not open hook %s: %s\n", path, dlerror());
                return ubik_raise(ERR_SYSTEM, "could not open hook");
        }

        /* Weird syntax alert: dlsym returns a function pointer disguised as a
         * void pointer. ISO C doesn't let you cast that to a function pointer,
         * so instead, we cast a reference to our function pointer to an
         * object-pointer-pointer, dereference that, and assign to it. */
        *((void **) &inst) = dlsym(dl, "__ubik_install");
        inst(&ubik_native_funcs);
        return OK;
}

struct ubik_native_record const_natives[] = {
        { "+", 2, "Number -> Number -> Number", NULL, NULL},
        { "-", 2, "Number -> Number -> Number", NULL, NULL},
        { "*", 2, "Number -> Number -> Number", NULL, NULL},
        { "/", 2, "Number -> Number -> Number", NULL, NULL},
        { "%", 2, "Number -> Number -> Number", NULL, NULL},
        { "rational-add", 2, "Number -> Number -> Number", NULL, NULL},
        { "rational-subtract", 2, "Number -> Number -> Number", NULL, NULL},
        { "rational-multiply", 2, "Number -> Number -> Number", NULL, NULL},
        { "rational-divide", 2, "Number -> Number -> Number", NULL, NULL},
        { "rational-remainder", 2, "Number -> Number -> Number", NULL, NULL},
        { "eq", 2, "a -> a -> Boolean", NULL, NULL},
        { "humanize", 1, "a -> String", NULL, NULL},
        { "concat", 2, "String -> String -> String", NULL, NULL},
        { "ubik-native-boolean-true", 0, "Boolean", NULL, NULL},
        { "ubik-native-boolean-false", 0, "Boolean", NULL, NULL},
        { "ubik-adt-ctor-matches?", 2, "String -> a -> Boolean", NULL, NULL},
        { "ubik-adt-get", 2, "Number -> a -> b", NULL, NULL},
#include "natives/adt-defs.h"
        { "ubik-multimethod-call-0", 0, NULL, NULL, NULL},
        { "ubik-multimethod-call-1", 0, NULL, NULL, NULL},
        { "ubik-multimethod-call-2", 0, NULL, NULL, NULL},
        { "ubik-multimethod-call-3", 0, NULL, NULL, NULL},
        { "ubik-multimethod-call-4", 0, NULL, NULL, NULL},
        { "ubik-multimethod-call-5", 0, NULL, NULL, NULL},
        { "ubik-multimethod-call-6", 0, NULL, NULL, NULL},
        { "ubik-multimethod-call-7", 0, NULL, NULL, NULL},
        { "ubik-multimethod-call-8", 0, NULL, NULL, NULL},
        { "ubik-multimethod-call-9", 0, NULL, NULL, NULL},
        { "ubik-multimethod-call-10", 0, NULL, NULL, NULL},
        { "ubik-multimethod-call-11", 0, NULL, NULL, NULL},
        { "ubik-multimethod-call-12", 0, NULL, NULL, NULL},
        { "ubik-multimethod-call-13", 0, NULL, NULL, NULL},
        { "ubik-multimethod-call-14", 0, NULL, NULL, NULL},
        { "ubik-multimethod-call-15", 0, NULL, NULL, NULL},
        { "ubik-multimethod-call-16", 0, NULL, NULL, NULL},
};

const size_t ubik_native_funcs_n =
        sizeof(const_natives) / sizeof(struct ubik_native_record);
struct ubik_vector ubik_native_funcs;

no_ignore ubik_error
ubik_natives_cache_types()
{
        size_t i;
        ubik_error err;
        struct ubik_native_record *r;
        for (i = 0; i < ubik_native_funcs_n; i++)
        {
                err = ubik_vector_append(
                        &ubik_native_funcs, &const_natives[i]);
                if (err != OK)
                        return err;
        }
        for (i = 0; i < ubik_native_funcs.n; i++)
        {
                r = (struct ubik_native_record *) ubik_native_funcs.elems[i];
                if (r->type_string == NULL)
                        continue;
                err = ubik_parse_type_expr(
                        &r->type_record, NULL, r->type_string);
                if (err != OK)
                {
                        printf("couldn't parse type for %s: %s\n",
                                r->name, r->type_string);
                        free(err);
                        continue;
                }
        }
        return OK;
}

bool
ubik_natives_is_defined(char *name)
{
        size_t i;
        struct ubik_native_record *n;
        for (i = 0; i < ubik_native_funcs.n; i++)
        {
                n = (struct ubik_native_record *)  ubik_native_funcs.elems[i];
                if (strcmp(n->name, name) == 0)
                        return true;
        }
        return false;
}

no_ignore ubik_error
ubik_natives_get_type(
        struct ubik_type_expr *res,
        char *name,
        struct ubik_alloc_region *r)
{
        struct ubik_native_record *n;
        size_t i;
        for (i = 0; i < ubik_native_funcs.n; i++)
        {
                n = (struct ubik_native_record *) ubik_native_funcs.elems[i];
                if (strcmp(n->name, name) != 0)
                        continue;
                if (n->type_record == NULL)
                        return ubik_raise(
                                ERR_UNKNOWN_TYPE,
                                "native function has undefined type");
                return ubik_type_expr_copy(res, n->type_record, r);
        }
        return ubik_raise(ERR_ABSENT, "native func undefined");
}

no_ignore ubik_error
ubik_natives_register(struct ubik_env *env, struct ubik_workspace *ws)
{
        struct ubik_native_record *n;
        struct ubik_value *ngraph;
        struct ubik_value *type;
        struct ubik_uri *uri;
        size_t i;
        ubik_error err;

        for (i = 0; i < ubik_native_funcs.n; i++)
        {
                n = (struct ubik_native_record *) ubik_native_funcs.elems[i];
                if (n->eval == NULL)
                        continue;
                printf("load native %s %s\n", n->name, n->type_string);

                ngraph = NULL;
                err = ubik_internal_native_create_op(
                        &ngraph, n->arity, n->eval, ws);
                if (err != OK)
                        return err;

                err = ubik_value_new(&type, ws);
                if (err != OK)
                        return err;
                type->gc.runtime_managed = true;
                type->type = UBIK_TYP;
                /* TODO: set type here */

                err = ubik_internal_native_uri(&uri, n->name);
                if (err != OK)
                        return err;

                err = ubik_env_set(env, uri, ngraph, type);
                ubik_uri_free(uri);
                if (err != OK)
                        return err;
        }

        err = _register_rational_add(env, ws);
        if (err != OK)
                return err;

        err = _register_rational_subtract(env, ws);
        if (err != OK)
                return err;

        err = _register_rational_multiply(env, ws);
        if (err != OK)
                return err;

        err = _register_rational_divide(env, ws);
        if (err != OK)
                return err;

        err = _register_rational_remainder(env, ws);
        if (err != OK)
                return err;

        err = _register_eq(env, ws);
        if (err != OK)
                return err;

        err = _register_humanize(env, ws);
        if (err != OK)
                return err;

        err = _register_concat(env, ws);
        if (err != OK)
                return err;

        err = _register_all_adt_new(env, ws);
        if (err != OK)
                return err;

        err = _register_adt_ctor_matches(env, ws);
        if (err != OK)
                return err;

        err = _register_adt_get(env, ws);
        if (err != OK)
                return err;

        err = _register_boolean_true(env, ws);
        if (err != OK)
                return err;

        err = _register_boolean_false(env, ws);
        if (err != OK)
                return err;

        return OK;
}
