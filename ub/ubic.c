/*
 * ubikc.c: ubik compiler
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


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ubik/compile.h"
#include "ubik/env.h"
#include "ubik/ubik.h"
#include "ubik/parse.h"
#include "ubik/schedule.h"
#include "ubik/value.h"

#define c(x) \
        do { \
        err = x; \
        if (err != OK) \
        { \
                char *expl = xl_error_explain(err); \
                printf("%s\n", expl); \
                free(err); free(expl); \
                goto teardown; \
        } } while(0)

void
usage()
{
        printf("ubikc compiles ubik source code to bytecode\n");
        printf("usage: ubikc SOURCE OUT\n");
}

int
main(int argc, char *argv[])
{
        struct xl_dagc **graphs = NULL;
        struct xl_stream in, out;
        size_t i;
        size_t n_graphs = 0;
        xl_error err;
        struct xl_compilation_env env;
        char *source_name;

        if (argc != 3)
        {
                usage();
                return EXIT_FAILURE;
        }

        c(xl_start());

        source_name = argv[1];
        if (xl_stream_rfile(&in, source_name) != OK)
        {
                printf("could not open %s for reading\n", argv[1]);
                return EXIT_FAILURE;
        }
        if (xl_stream_wfile(&out, argv[2]) != OK)
        {
                printf("could not open %s for writing\n", argv[2]);
                return EXIT_FAILURE;
        }

        c(xl_compile_env_default(&env));
        c(xl_compile(&graphs, &n_graphs, source_name, &in, &env));
        c(xl_compile_env_free(&env));
        c(xl_save(&out, graphs, n_graphs));

teardown:
        xl_stream_close(&in);
        xl_stream_close(&out);

        for (i = 0; i < n_graphs; i++)
        {
                if (xl_release(graphs[i]) != OK)
                        printf("error when releasing graph\n");
        }

        free(graphs);

        if (xl_teardown() != OK)
                printf("error when tearing down runtime\n");

        return err == OK ? EXIT_SUCCESS : EXIT_FAILURE;
}

