/*
 * ubik-dis.c: ubik disassembler
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

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

#include "ubik/assert.h"
#include "ubik/bytecode.h"
#include "ubik/stream.h"
#include "ubik/string.h"
#include "ubik/ubik.h"
#include "ubik/util.h"

#define CHECK_ERR(msg)                                          \
        do { if (err != OK) {                                   \
                        char *expl = ubik_error_explain(err);   \
                        printf(msg ": %s\n", expl);             \
                        free(err); free(expl);                  \
                        return 1;                               \
                } } while(0)

void
dis(struct ubik_stream *s, struct ubik_value *v)
{
        ubik_word i;

        if (v->type == UBIK_NOV)
                return;
        ubik_fprintf(s, "\n%" PRIdPTR "\n", v->gc.id);

        switch (v->type)
        {
        case UBIK_STR:
                ubik_fprintf(s, "STR\n%" PRIu64 "\n", v->str.length);
                ubik_assert(
                        ubik_stream_write(s, v->str.data, v->str.length)
                        == v->str.length);
                ubik_fprintf(s, "\n");
                return;

        case UBIK_RAT:
                ubik_fprintf(s, "RAT\n%" PRId64 "\n%" PRIu64 "\n",
                             v->rat.num, v->rat.den);
                return;

        case UBIK_TUP:
                ubik_fprintf(s, "TUP\n%" PRIu64 "\n", v->tup.n);
                for (i = 0; i < v->tup.n; i++)
                        ubik_fprintf(s, "%" PRIu64 " %" PRIu64 "\n",
                                     v->tup.elems[i]->gc.id,
                                     v->tup.types[i]->gc.id);
                return;

        case UBIK_FUN:
                ubik_fprintf(s, "FUN\n");
                return;

        case UBIK_MUL:
                ubik_fprintf(s, "MUL\n");
                return;

        case UBIK_TYP:
                ubik_fprintf(s, "TYP\n");
                switch (v->typ.t)
                {
                case UBIK_TYPE_STR:
                        ubik_fprintf(s, "str\n");
                        return;
                case UBIK_TYPE_RAT:
                        ubik_fprintf(s, "rat\n");
                        return;
                case UBIK_TYPE_BOO:
                        ubik_fprintf(s, "boo\n");
                        return;
                case UBIK_TYPE_ADT:
                        ubik_fprintf(s, "adt\n");
                        return;
                default:
                        ubik_fprintf(s, "unknown %d\n", v->typ.t);
                        return;
                }

        case UBIK_IMP:
                ubik_fprintf(s, "IMP\n");
                return;

        case UBIK_BOO:
                ubik_fprintf(s, "BOO\n%s\n", v->boo.value ? "true" : "false");
                return;

        case UBIK_PAP:
                ubik_fprintf(s, "PAP\n");
                return;

        case UBIK_NOV:
                ubik_unreachable("no-value dis");
        case UBIK_MAX_VALUE_TYPE:
        default:
                ubik_unreachable("bad value type in workspace");
        }
}

int
main(int argc, char *argv[])
{
        char *fname;
        struct ubik_stream stream;
        struct ubik_stream sstdout;
        struct ubik_workspace *ws;
        struct ubik_workspace *root;
        ubik_error err;
        size_t i, j;

        if (argc <= 1)
        {
                fprintf(stderr, "usage: %s path/to/bytecode.ub\n", argv[0]);
                return 1;
        }
        fname = argv[1];

        err = ubik_stream_rfile(&stream, fname);
        CHECK_ERR("open input");

        err = ubik_stream_wfilep(&sstdout, stdout);
        CHECK_ERR("open stdout");

        err = ubik_bytecode_read(&root, &stream);
        CHECK_ERR("read bytecode");

        for (ws = root, j = 0; ws != NULL; ws = ws->next)
        {
                for (i = 0; i < ws->n; i++, j++)
                        ws->values[i].gc.id = j;
        }

        for (ws = root; ws != NULL; ws = ws->next)
        {
                for (i = 0; i < ws->n; i++)
                        dis(&sstdout, &ws->values[i]);
        }

        ubik_workspace_free(root);
        return 0;
}

