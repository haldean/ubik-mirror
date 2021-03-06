/*
 * adt/hook.c: built-in native methods for ADTs
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
#include "ubik/hooks.h"
#include "ubik/rttypes.h"
#include "ubik/string.h"
#include "ubik/ubik.h"
#include "ubik/util.h"
#include "ubik/value.h"

#include <inttypes.h>
#include <string.h>

DEF_EVALUATOR(adt_new)
{
        struct ubik_value *type_decl;
        struct ubik_value *ctor;
        struct ubik_value *cargs;
        struct ubik_value *res;
        ubik_error err;
        size_t i;

        type_decl = args[0];
        ctor = args[1];

        err = ubik_value_new(&cargs, ws);
        if (err != OK)
                return err;
        cargs->gc.runtime_managed = true;

        cargs->type = UBIK_TUP;
        cargs->tup.n = fun->fun.n - 3;
        ubik_galloc(
                (void**) &cargs->tup.elems,
                cargs->tup.n, sizeof(struct ubik_value *));
        ubik_galloc(
                (void**) &cargs->tup.types,
                cargs->tup.n, sizeof(struct ubik_value *));

        for (i = 2; i < fun->fun.n - 1; i++)
        {
                cargs->tup.elems[i - 2] = args[i];
                cargs->tup.types[i - 2] = argtypes[i];
        }

        err = ubik_value_new(&res, ws);
        if (err != OK)
                return err;
        res->gc.runtime_managed = true;
        err = ubik_adt_instantiate(
                res, type_decl, ctor, cargs, ws);
        if (err != OK)
                return err;
        *res_ref = res;
        *res_type = type_decl;
        return OK;
}

DEF_EVALUATOR(ctor_matches)
{
        struct ubik_value *inst;
        struct ubik_value *match_name;
        struct ubik_value *ctor_name;
        struct ubik_value *res;
        bool matches;
        ubik_error err;

        match_name = args[0];
        inst = args[1];

        err = ubik_adt_get_ctor(&ctor_name, inst);
        if (err != OK)
                return err;

        matches = ubik_value_eq(match_name, ctor_name);

        err = ubik_value_new(&res, ws);
        if (err != OK)
                return err;
        res->gc.runtime_managed = true;
        res->type = UBIK_BOO;
        res->boo.value = matches;
        *res_ref = res;

        err = ubik_value_new(res_type, ws);
        if (err != OK)
                return err;
        (*res_type)->gc.runtime_managed = true;
        err = ubik_type_boo(*res_type);
        if (err != OK)
                return err;

        return OK;
}

DEF_EVALUATOR(adt_get)
{
        struct ubik_value *inst;
        struct ubik_value *index_val;
        struct ubik_value *res;
        ubik_word index;
        ubik_error err;

        index_val = args[0];
        inst = args[1];

        ubik_assert(index_val->type == UBIK_RAT && index_val->rat.den == 1);
        index = index_val->rat.num;

        err = ubik_adt_get_field(&res, inst, index);
        if (err != OK)
                return err;
        err = ubik_adt_get_field_type(res_type, inst, index);
        if (err != OK)
                return err;

        *res_ref = res;
        return OK;
}

#define TYPEBUF_SIZE 512

ubik_error
__ubik_install(struct ubik_vector *hooks, struct ubik_alloc_region *region)
{
        struct ubik_hook *r;
        ubik_error err;
        size_t i;
        size_t j;
        size_t k;
        char typebuf[TYPEBUF_SIZE];
        char *funcname;

        ubik_alloc1(&r, struct ubik_hook, region);
        *r = (struct ubik_hook) {
                "ubik-adt-ctor-matches?", 2,
                "ubik:String -> ubik:a -> ubik:Boolean", NULL, ctor_matches
        };
        err = ubik_vector_append(hooks, r);
        if (err != OK)
                return err;

        ubik_alloc1(&r, struct ubik_hook, region);
        *r = (struct ubik_hook) {
                "ubik-adt-get", 2, "ubik:Number -> ubik:a -> ubik:b", NULL, adt_get
        };
        err = ubik_vector_append(hooks, r);
        if (err != OK)
                return err;

        for (i = 0; i < UBIK_MAX_ADT_FIELDS; i++)
        {
                memset(typebuf, 0x00, TYPEBUF_SIZE);
                strcat(typebuf, "ubik:t -> ubik:String");
                for (j = 0; j < i; j++)
                {
                        strcat(typebuf, " -> ubik:arg-0");
                        k = strlen(typebuf) - 1;
                        /* assigns a simple single-char encoding to
                         * each argument, counting from 0 to 9, then a
                         * to z, then A to Z. */
                        if (j < 10) typebuf[k] += j;
                        else if (j < 26 + 10) typebuf[k] = 'a' + j - 10;
                        else if (j < 26 + 26 + 10) typebuf[k] = 'A' + j - 36;
                        else return ubik_raise(
                                ERR_SYSTEM, "too many adt fields to encode");
                }

                ubik_asprintf(&funcname, region, "ubik-adt-new-%" PRIu64, i);

                ubik_alloc1(&r, struct ubik_hook, region);
                *r = (struct ubik_hook) {
                        funcname,
                        i + 2,
                        ubik_strdup(typebuf, region),
                        NULL,
                        adt_new
                };
                err = ubik_vector_append(hooks, r);
                if (err != OK)
                        return err;
        }

        return OK;
}
