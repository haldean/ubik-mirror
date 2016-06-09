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
                char *expl = ubik_error_explain(err); \
                printf("%s\n", expl); \
                free(err); free(expl); \
                goto teardown; \
        } } while(0)


static struct ubik_stream out;

void
usage()
{
        printf("ubikc compiles ubik source code to bytecode\n");
        printf("usage: ubikc SOURCE OUT\n");
}

no_ignore ubik_error
save_result(const struct ubik_compile_result *res)
{
        return ubik_save(&out, res->graphs, res->n_graphs);
}

int
main(int argc, char *argv[])
{
        struct ubik_compile_env env = {0};
        struct ubik_compile_request req;
        struct ubik_stream in;
        ubik_error err;

        if (argc != 3)
        {
                usage();
                return EXIT_FAILURE;
        }

        c(ubik_start());

        req.source_name = argv[1];
        req.reason = LOAD_MAIN;
        req.cb = save_result;
        req.source = &in;
        if (ubik_stream_rfile(req.source, req.source_name) != OK)
        {
                printf("could not open %s for reading\n", argv[1]);
                return EXIT_FAILURE;
        }
        if (ubik_stream_wfile(&out, argv[2]) != OK)
        {
                printf("could not open %s for writing\n", argv[2]);
                return EXIT_FAILURE;
        }

        c(ubik_compile_env_default(&env));
        c(ubik_compile_enqueue(&env, &req));
        c(ubik_compile_run(&env));

teardown:
        ubik_stream_close(req.source);
        ubik_stream_close(&out);

        if (ubik_compile_env_free(&env) != OK)
                printf("error when freeing compile env\n");

        if (ubik_teardown() != OK)
                printf("error when tearing down runtime\n");

        return err == OK ? EXIT_SUCCESS : EXIT_FAILURE;
}

