/*
 * expelc.c: expel compiler
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
#include <unistd.h>

#include "expel/compile.h"
#include "expel/env.h"
#include "expel/expel.h"
#include "expel/parse.h"
#include "expel/schedule.h"
#include "expel/value.h"

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
        printf("expelc compiles expel source code to bytecode\n");
        printf("usage: expelc SOURCE OUT\n");
}

int
main(int argc, char *argv[])
{
        struct xl_dagc **graphs;
        struct xl_stream in, out;
        size_t n_graphs;
        xl_error err;
        char scratch_dir[512];

        if (argc != 3)
        {
                usage();
                return EXIT_FAILURE;
        }

        if (getcwd(scratch_dir, 500) == NULL)
        {
                perror("could not open current directory");
                return EXIT_FAILURE;
        }
        strcat(scratch_dir, "/expel-build");

        c(xl_start());

        if (xl_stream_rfile(&in, argv[1]) != OK)
        {
                printf("could not open %s for reading\n", argv[1]);
                return EXIT_FAILURE;
        }
        if (xl_stream_wfile(&out, argv[2]) != OK)
        {
                printf("could not open %s for writing\n", argv[2]);
                return EXIT_FAILURE;
        }

        c(xl_compile(&graphs, &n_graphs, &in, scratch_dir));
        c(xl_save(&out, graphs, n_graphs));

teardown:
        xl_stream_close(&in);
        xl_stream_close(&out);

        if (xl_teardown() != OK)
                printf("error when tearing down runtime\n");

        return err == OK ? EXIT_SUCCESS : EXIT_FAILURE;
}

