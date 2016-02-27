/*
 * pyasm.c: pyasm test runner
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
#include "expel/schedule.h"
#include "expel/stream.h"
#include "expel/timer.h"
#include "expel/util.h"
#include "expel/value.h"

#define CHECK_ERR(msg) \
        do { if (err != OK) \
        { \
                char *expl = xl_error_explain(err); \
                printf(msg ": %s\n", expl); \
                free(err); free(expl); \
                goto teardown; \
        } } while(0)

xl_error
test_file(char *fname)
{
        struct xl_stream stream;
        struct xl_stream sstdout;
        struct xl_dagc **graphs;
        struct xl_env env;
        struct xl_scheduler *s;
        struct xl_value *expected, *actual;
        size_t n_graphs, i, modinit_i;
        xl_error err, teardown_err;
        struct xl_timer *timer;
        int64_t elapsed;

        err = OK;
        n_graphs = 0;
        graphs = NULL;

        printf("%s\n", fname);

        err = xl_timer_new(&timer);
        CHECK_ERR("couldn't create timer");

        err = xl_timer_start(timer);
        CHECK_ERR("couldn't start timer");

        err = xl_start();
        CHECK_ERR("couldn't start expel");

        err = xl_stream_rfile(&stream, fname);
        CHECK_ERR("couldn't open file");

        err = xl_stream_wfilep(&sstdout, stdout);
        CHECK_ERR("couldn't open stdout");

        err = xl_load(&graphs, &n_graphs, &stream);
        CHECK_ERR("couldn't load file");

        modinit_i = 0;
        for (i = 0; i < n_graphs; i++)
        {
                if (graphs[i]->tag & TAG_GRAPH_MODINIT)
                {
                        modinit_i = i;
                        break;
                }
        }

        err = xl_timer_elapsed(&elapsed, timer);
        CHECK_ERR("couldn't read timer");
        printf("\ttime from start to loaded:    %ld usec\n", elapsed);

        err = xl_value_new(&expected);
        CHECK_ERR("couldn't create expected value");

        err = xl_value_load(expected, &stream);
        if (err != OK && err->error_code == ERR_NO_DATA)
        {
                /* No expected result for this run, we'll just run it and make
                 * sure we don't crash. */
                err = xl_release(expected);
                CHECK_ERR("couldn't release expected");
                expected = NULL;
        }
        else
                CHECK_ERR("couldn't load expected");

        err = xl_env_init(&env);
        CHECK_ERR("couldn't create environment");

        err = xl_schedule_new(&s);
        CHECK_ERR("couldn't create scheduler");

        err = xl_schedule_push(s, graphs[modinit_i], &env, NULL);
        CHECK_ERR("couldn't push graph into scheduler");

        err = xl_schedule_run(s);
        CHECK_ERR("couldn't run scheduler");

        err = xl_timer_elapsed(&elapsed, timer);
        CHECK_ERR("couldn't read timer");
        printf("\ttime from start to evaluated: %ld usec\n", elapsed);

        err = xl_env_free(&env);
        CHECK_ERR("couldn't free environment");

        if (expected != NULL)
        {
                actual = graphs[modinit_i]->result->known.tree;
                printf("\texpected:  ");
                err = xl_value_print(&sstdout, expected);
                CHECK_ERR("couldn't print expected");

                printf("\n\t  actual:  ");
                if (actual != NULL)
                {
                        err = xl_value_print(&sstdout, actual);
                        CHECK_ERR("couldn't print actual");
                }
                else
                {
                        printf("not evaluated");
                }
                printf("\n");

                if (actual == NULL || !xl_value_eq(expected, actual))
                {
                        printf("FAIL\n");
                        err = xl_raise(ERR_TEST_FAILED, NULL);
                        goto teardown;
                }
        }

teardown:
        free(timer);

        if (graphs != NULL)
        {
                for (i = 0; i < n_graphs; i++)
                {
                        if (graphs[i] == NULL)
                                continue;
                        teardown_err = xl_release(graphs[i]);
                        if (teardown_err != OK)
                        {
                                char *explain = xl_error_explain(teardown_err);
                                printf("graph release failed: %s\n", explain);
                                free(explain);
                                free(teardown_err);
                        }

                }
                free(graphs);
        }

        teardown_err = xl_teardown();
        if (teardown_err != OK)
        {
                char *explain = xl_error_explain(teardown_err);
                printf("teardown failed: %s\n", explain);
                free(explain);
                free(teardown_err);
        }

        xl_stream_close(&stream);

        if (err == OK)
                printf("OK\n");
        printf("\n");

        return err;
}

int
main(int argc, char *argv[])
{
        uint32_t n_failures;
        int i;

        n_failures = 0;
        for (i = 1; i < argc; i++)
        {
                if (test_file(argv[i]) != OK)
                        n_failures++;
        }

        if (n_failures)
                printf("%d of %d failed\n", n_failures, argc - 1);
        else
                printf("all %d succeeded\n", argc - 1);
        return n_failures;
}
