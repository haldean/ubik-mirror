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
#include "ubik/dagc.h"
#include "ubik/env.h"
#include "ubik/ubik.h"
#include "ubik/schedule.h"
#include "ubik/stream.h"
#include "ubik/timer.h"
#include "ubik/util.h"
#include "ubik/value.h"

#define CHECK_ERR(msg, label) \
        do { if (err != OK) \
        { \
                char *expl = ubik_error_explain(err); \
                printf(msg ": %s\n", expl); \
                free(err); free(expl); \
                goto label; \
        } } while(0)

ubik_error
test_file(char *fname, bool debug, bool timing)
{
        struct ubik_stream stream;
        struct ubik_stream sstdout;
        struct ubik_dagc **graphs;
        struct ubik_env env;
        struct ubik_scheduler *s;
        struct ubik_value *expected, *actual;
        size_t n_graphs, i, modinit_i;
        ubik_error err, teardown_err;
        struct ubik_timer *timer;
        int64_t elapsed;
        bool pushed_modinit;

        err = OK;
        n_graphs = 0;
        graphs = NULL;
        s = NULL;

        if (timing)
        {
                err = ubik_timer_new(&timer);
                CHECK_ERR("couldn't create timer", teardown);
                err = ubik_timer_start(timer);
                CHECK_ERR("couldn't start timer", teardown);
        }

        err = ubik_start();
        CHECK_ERR("couldn't start ubik", teardown);

        err = ubik_stream_rfile(&stream, fname);
        CHECK_ERR("couldn't open file", teardown);

        err = ubik_stream_wfilep(&sstdout, stdout);
        CHECK_ERR("couldn't open stdout", teardown);

        err = ubik_load(&graphs, &n_graphs, &stream);
        CHECK_ERR("couldn't load file", teardown);

        ubik_assert(n_graphs != 0);

        if (timing)
        {
                err = ubik_timer_elapsed(&elapsed, timer);
                CHECK_ERR("couldn't read timer", teardown);
                printf("\ttime from start to loaded:    %" PRId64 " usec\n", elapsed);
        }

        if (debug)
        {
                err = ubik_value_new(&expected);
                CHECK_ERR("couldn't create expected value", teardown);

                err = ubik_value_load(expected, &stream);
                if (err != OK && err->error_code == ERR_NO_DATA)
                {
                        /* No expected result for this run, we'll just run it and make
                         * sure we don't crash. */
                        err = ubik_release(expected);
                        CHECK_ERR("couldn't release expected", teardown);
                        expected = NULL;
                }
                else
                        CHECK_ERR("couldn't load expected", teardown);
        }

        err = ubik_env_init(&env);
        CHECK_ERR("couldn't create environment", teardown);

        err = ubik_schedule_new(&s);
        CHECK_ERR("couldn't create scheduler", teardown);

        modinit_i = 0;
        pushed_modinit = false;
        for (i = 0; i < n_graphs; i++)
        {
                if (graphs[i]->tag & TAG_GRAPH_MODINIT)
                {
                        err = ubik_schedule_push(s, graphs[i], &env, NULL);
                        CHECK_ERR("couldn't push graph into scheduler", teardown);

                        modinit_i = i;
                        pushed_modinit = true;
                }
        }
        ubik_assert(pushed_modinit);

        err = ubik_schedule_run(s);
        CHECK_ERR("couldn't run scheduler", teardown);

        err = ubik_env_free(&env);
        CHECK_ERR("couldn't free environment", teardown);

        if (timing)
        {
                err = ubik_timer_elapsed(&elapsed, timer);
                CHECK_ERR("couldn't read timer", teardown);
                printf("\ttime from start to evaluated: %" PRId64 " usec\n", elapsed);
        }

        if (debug && expected != NULL)
        {
                actual = graphs[modinit_i]->result->known.tree;

                if (actual == NULL || !ubik_value_eq(expected, actual))
                {
                        printf("fail: %s\n\texpected:  ", fname);
                        err = ubik_value_print(&sstdout, expected);
                        CHECK_ERR("couldn't print expected", teardown);

                        printf("\n\t  actual:  ");
                        if (actual != NULL)
                        {
                                err = ubik_value_print(&sstdout, actual);
                                CHECK_ERR("couldn't print actual", teardown);
                        }
                        else
                        {
                                printf("not evaluated");
                        }
                        printf("\n");
                        err = ubik_raise(ERR_TEST_FAILED, NULL);
                        goto teardown;
                }
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

        if (graphs != NULL)
        {
                for (i = 0; i < n_graphs; i++)
                {
                        if (graphs[i] == NULL)
                                continue;
                        teardown_err = ubik_release(graphs[i]);
                        if (teardown_err != OK)
                        {
                                char *explain = ubik_error_explain(teardown_err);
                                printf("graph release failed: %s\n", explain);
                                free(explain);
                                if (err == OK)
                                        err = teardown_err;
                                else
                                        free(teardown_err);
                        }

                }
                free(graphs);
        }

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

        ubik_stream_close(&stream);

        if (err == OK)
                printf("ok:   %s\n", fname);

        return err;
}

int
main(int argc, char *argv[])
{
        uint32_t n_failures;
        int i;
        char *debug_opt;
        bool debug;
        char *timing_opt;
        bool timing;

        debug_opt = getenv("UBIK_DEBUG");
        debug = debug_opt != NULL && strlen(debug_opt) > 0;

        timing_opt = getenv("UBIK_TIMING");
        timing = timing_opt != NULL && strlen(timing_opt) > 0;

        n_failures = 0;
        for (i = 1; i < argc; i++)
        {
                if (test_file(argv[i], debug, timing) != OK)
                        n_failures++;
        }

        if (debug && n_failures)
                printf("%d of %d failed\n", n_failures, argc - 1);
        return n_failures;
}
