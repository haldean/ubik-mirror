/*
 * ubik.c: ubik virtual machine
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

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ubik/assert.h"
#include "ubik/bytecode.h"
#include "ubik/env.h"
#include "ubik/rt.h"
#include "ubik/schedule.h"
#include "ubik/stream.h"
#include "ubik/timer.h"
#include "ubik/ubik.h"
#include "ubik/util.h"
#include "ubik/value.h"

#define CHECK_ERR(msg)                                          \
        do { if (err != OK) {                                   \
                        char *expl = ubik_error_explain(err);   \
                        printf(msg ": %s\n", expl);             \
                        free(err); free(expl);                  \
                        goto teardown;                          \
                } } while(0)

ubik_error
run_file(char *fname, bool timing)
{
        struct ubik_stream stream;
        struct ubik_stream sstdout;
        struct ubik_workspace *ws;
        struct ubik_env env;
        struct ubik_scheduler *s;
        ubik_error err, teardown_err;
        struct ubik_timer *timer;
        int64_t elapsed;

        ws = NULL;
        s = NULL;

        if (timing)
        {
                err = ubik_timer_new(&timer);
                CHECK_ERR("couldn't create timer");
                err = ubik_timer_start(timer);
                CHECK_ERR("couldn't start timer");
        }

        err = ubik_stream_rfile(&stream, fname);
        CHECK_ERR("couldn't open file");

        err = ubik_stream_wfilep(&sstdout, stdout);
        CHECK_ERR("couldn't open stdout");

        err = ubik_bytecode_read(&ws, &stream);
        CHECK_ERR("couldn't load file");

        err = ubik_start(ws);
        CHECK_ERR("couldn't start ubik");

        if (timing)
        {
                err = ubik_timer_elapsed(&elapsed, timer);
                CHECK_ERR("couldn't read timer");
                printf("time from start to loaded:    %" PRId64 " usec\n", elapsed);
        }

        err = ubik_env_init(&env);
        CHECK_ERR("couldn't create environment");

        err = ubik_schedule_new(&s);
        CHECK_ERR("couldn't create scheduler");

        err = ubik_schedule_push_roots(s, &env, ws);
        CHECK_ERR("couldn't schedule root objects");

        err = ubik_schedule_run(s);
        CHECK_ERR("couldn't run scheduler");

        if (timing)
        {
                err = ubik_timer_elapsed(&elapsed, timer);
                CHECK_ERR("couldn't read timer");
                printf("time from start to evaluated: %" PRId64 " usec\n", elapsed);
        }

teardown:
        if (timing)
                free(timer);

        if (s != NULL)
        {
                teardown_err = ubik_schedule_free(s);
                if (teardown_err != OK)
                {
                        char *explain = ubik_error_explain(teardown_err);
                        printf("scheduler free failed: %s\n", explain);
                        free(explain);
                        free(teardown_err);
                }
                free(s);
        }

        teardown_err = ubik_env_free(&env);
        if (teardown_err != OK)
        {
                char *explain = ubik_error_explain(teardown_err);
                printf("env free failed: %s\n", explain);
                free(explain);
                free(teardown_err);
        }

        if (ws != NULL)
                ubik_workspace_free(ws);

        teardown_err = ubik_teardown();
        if (teardown_err != OK)
        {
                char *explain = ubik_error_explain(teardown_err);
                printf("teardown failed: %s\n", explain);
                free(explain);
                if (err == OK)
                        err = teardown_err;
                else
                        free(teardown_err);
        }

        fflush(stdout);
        ubik_stream_close(&stream);
        return err;
}

int
main(int argc, char *argv[])
{
        char *timing_opt;
        bool timing;

        timing_opt = getenv("UBIK_TIMING");
        timing = timing_opt != NULL && strlen(timing_opt) > 0;

        if (argc == 1)
        {
                printf("no bytecode files given, aborting\n");
                return 1;
        }
        return (run_file(argv[1], timing) == OK) ? 0 : 2;
}
