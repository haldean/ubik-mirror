/*
 * testing.c: tools for Ubik tests
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

#include "ubik/env.h"
#include "ubik/testing.h"
#include "ubik/schedule.h"

no_ignore ubik_error
ubik_testing_run(struct ubik_compile_result *compiled)
{
        struct ubik_scheduler *sched;
        struct ubik_ast_test *test;
        struct ubik_value *v;
        struct ubik_env env;
        struct ubik_workspace *ws;
        size_t i;
        ubik_error err;

        err = ubik_schedule_new(&sched);
        if (err != OK)
                return err;

        err = ubik_env_init(&env);
        if (err != OK)
                return err;

        err = ubik_schedule_push_roots(
                sched, &env, compiled->request->workspace);
        if (err != OK)
                return err;

        for (i = 0; i < compiled->ast->tests.n; i++)
        {
                test = (struct ubik_ast_test *) compiled->ast->tests.elems[i];

                err = compile_test(&v, test, ws);
                if (err != OK)
                        return err;

                err = ubik_schedule_push(sched, v, &env, false, notify, ws);
                if (err != OK)
                        return err;
        }

        err = ubik_schedule_run(sched);
        if (err != OK)
                return err;

        err = ubik_schedule_free(sched);
        if (err != OK)
                return err;

        err = ubik_env_free(&env);
        if (err != OK)
                return err;

        ubik_workspace_free(ws);
        if (err != OK)
                return err;

        return OK;
}

