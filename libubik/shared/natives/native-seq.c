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
#include "ubik/schedule.h"
#include "ubik/str.h"
#include "ubik/ubik.h"
#include "ubik/util.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static ubik_error
_native_concat(struct ubik_exec_graph *gexec)
{
        struct ubik_value *res;
        ubik_error err;

        err = ubik_value_new(&res, gexec->workspace);
        if (err != OK)
                return err;

        ubik_str_concat(res, gexec->nv[0], gexec->nv[1]);
        gexec->nv[2] = res;
        gexec->nt[2] = gexec->nv[0];

        return OK;
}

#define DEF_BINARY
#define DEF_OP concat
#define DEF_ARG_TYPE ubik_type_string
#define DEF_OP_EVAL _native_concat
#define DEF_OP_URI "concat"
#include "ubik/def-native.h"
