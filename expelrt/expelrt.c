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

int
main(int argc, char *argv[])
{
        struct xl_stream stream;
        struct xl_dagc graph;
        struct xl_env env;
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

        err = xl_load(&graph, &stream);
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

        err = xl_dagc_eval(&env, &graph);
        if (err != OK)
        {
                fprintf(stderr, "couldn't eval dagc: %s\n",
                        xl_explain_error(err));
                return EXIT_FAILURE;
        }

        size_t i;
        for (i = 0; i < graph.n; i++)
        {
                printf("%lu: ", i);
                if ((graph.nodes[i]->flags & XL_DAGC_FLAG_COMPLETE) == 0)
                        printf("not evaluated\n");
                else if (graph.nodes[i]->known_value == NULL)
                        printf("no value\n");
                else
                        printf("left %lu\n", graph.nodes[i]->known_value->left.v);
        }

        return EXIT_SUCCESS;
}
