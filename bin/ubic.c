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

#include "ubik/alloc.h"
#include "ubik/bytecode.h"
#include "ubik/compile.h"
#include "ubik/env.h"
#include "ubik/parse.h"
#include "ubik/schedule.h"
#include "ubik/ubik.h"
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
usage(char *argv[])
{
        printf(
"ubic compiles ubik source code to bytecode\n"
"usage: %s SOURCE [OUT]\n\n"
"If OUT is not specified, the program is fully compiled and the result is\n"
"thrown away. This is useful if you want to know if a program compiles\n"
"correctly, but you don't care about the result.\n", argv[0]);
}

no_ignore ubik_error
save_result(const struct ubik_compile_result *res)
{
        return ubik_bytecode_write(&out, res->request->workspace);
}

int
main(int argc, char *argv[])
{
        struct ubik_compile_env env = {0};
        struct ubik_compile_request req = {0};
        struct ubik_stream feedback;
        struct ubik_workspace *ws;
        bool discard_res;
        ubik_error err;
        ubik_error rerr;

        if (argc < 2 || argc > 3)
        {
                usage(argv);
                return EXIT_FAILURE;
        }
        discard_res = argc == 2;
        err = OK;

        if (ubik_stream_wfilep(&feedback, stderr) != OK)
        {
                printf("couldn't open stdout for writing\n");
                return EXIT_FAILURE;
        }

        c(ubik_workspace_new(&ws));
        c(ubik_start(ws));

        req.source_name = argv[1];
        req.reason = LOAD_MAIN;
        if (discard_res)
                req.cb = NULL;
        else
                req.cb = save_result;
        req.feedback = &feedback;
        req.workspace = ws;

        if (strcmp(req.source_name, "-") == 0)
        {
                req.source_name = "(stdin)";
                if (ubik_stream_rfilep(&req.source, stdin) != OK)
                {
                        printf("could not open stdin for reading\n");
                        return EXIT_FAILURE;
                }
        }
        else
        {
                if (ubik_stream_rfile(&req.source, req.source_name) != OK)
                {
                        printf("could not open %s for reading\n", argv[1]);
                        return EXIT_FAILURE;
                }
        }
        if (!discard_res && ubik_stream_wfile(&out, argv[2]) != OK)
        {
                printf("could not open %s for writing\n", argv[2]);
                return EXIT_FAILURE;
        }

        c(ubik_compile_env_default(&env, ws));
        c(ubik_compile_enqueue(&env, &req));
        c(ubik_compile_run(&env));

teardown:
        ubik_stream_close(&req.source);
        ubik_stream_close(&out);

        if ((rerr = ubik_compile_env_free(&env)) != OK)
                printf("error when freeing compile env: %s\n",
                       ubik_error_explain(rerr));

        if ((rerr = ubik_teardown()) != OK)
                printf("error when tearing down runtime: %s\n",
                       ubik_error_explain(rerr));

        ubik_workspace_free(ws);

        return err == OK ? EXIT_SUCCESS : EXIT_FAILURE;
}

