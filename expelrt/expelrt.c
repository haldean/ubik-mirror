/*
 * expel.h: expel base types
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

#include <stdio.h>
#include <stdlib.h>

#include "expel/dagc.h"
#include "expel/env.h"
#include "expel/expel.h"
#include "expel/stream.h"
#include "expel/util.h"

int
main(int argc, char *argv[])
{
        struct xl_stream stream;
        struct xl_dagc *graphs;
        struct xl_env env;
        size_t n_graphs;
        xl_error_t err;

        if (argc <= 1)
        {
                fprintf(stderr, "missing expel file\n");
                return EXIT_FAILURE;
        }

        err = xl_start();
        if (err != OK)
        {
                fprintf(stderr, "couldn't start expel: %s\n",
                        xl_explain_error(err));
                return EXIT_FAILURE;
        }

        err = xl_stream_rfile(&stream, argv[1]);
        if (err != OK)
        {
                fprintf(stderr, "couldn't open %s: %s\n",
                        argv[1], xl_explain_error(err));
                return EXIT_FAILURE;
        }

        err = xl_load(&graphs, &n_graphs, &stream);
        if (err != OK)
        {
                fprintf(stderr, "couldn't load %s: %s\n",
                        argv[1], xl_explain_error(err));
                return EXIT_FAILURE;
        }

        err = xl_env_init(&env);
        if (err != OK)
        {
                fprintf(stderr, "couldn't create environment: %s\n",
                        xl_explain_error(err));
                return EXIT_FAILURE;
        }

        err = xl_dagc_eval(&env, graphs);
        if (err != OK)
        {
                fprintf(stderr, "couldn't eval dagc: %s\n",
                        xl_explain_error(err));
                return EXIT_FAILURE;
        }

        size_t i;
        for (i = 0; i < graphs->n; i++)
        {
                printf("% 4d %hx %s: ", (int) i,
                       (short)((uintptr_t) graphs->nodes[i]),
                       xl_explain_word(graphs->nodes[i]->node_type));
                if ((graphs->nodes[i]->flags & XL_DAGC_FLAG_COMPLETE) == 0)
                        printf("not evaluated\n");
                else if (graphs->nodes[i]->value_type == DAGC_TYPE_GRAPH)
                        printf("graph\n");
                else if (graphs->nodes[i]->known.tree == NULL)
                        printf("no value\n");
                else
                        printf("left %lu\n",
                               graphs->nodes[i]->known.tree->left.v);
        }

        return EXIT_SUCCESS;
}
