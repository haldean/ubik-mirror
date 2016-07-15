/*
 * native-seq.c: built-in native methods
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

#include "ubik/env.h"
#include "ubik/natives.h"
#include "ubik/ubik.h"
#include "ubik/util.h"
#include "ubik/value.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static ubik_error
_native_concat(struct ubik_env *env, struct ubik_dagc *graph)
{
        ubik_error err;
        struct ubik_value *res;
        char *str0, *str1, *concat;
        size_t n0, n1, n;

        unused(env);

        err = ubik_string_read(&str0, &n0, graph->nodes[0]->known.tree);
        if (err != OK)
                return err;
        err = ubik_string_read(&str1, &n1, graph->nodes[1]->known.tree);
        if (err != OK)
                return err;

        n = n0 + n1;
        concat = calloc(n + 1, sizeof(char));
        if (concat == NULL)
                return ubik_raise(ERR_NO_MEMORY, "concat alloc");
        memcpy(concat, str0, n0);
        memcpy(&concat[n0], str1, n1);

        err = ubik_value_new(&res);
        if (err != OK)
                return err;
        err = ubik_value_pack_string(res, concat, n);
        if (err != OK)
                return err;

        free(str0);
        free(str1);
        free(concat);

        graph->result->known.tree = res;
        graph->result->known_type = graph->nodes[0]->known_type;

        err = ubik_take(graph->result->known_type);
        if (err != OK)
                return err;

        return OK;
}

#define DEF_BINARY
#define DEF_OP concat
#define DEF_ARG_TYPE ubik_type_string
#define DEF_OP_EVAL _native_concat
#define DEF_OP_URI "concat"
#include "ubik/def-native.h"
