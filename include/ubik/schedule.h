/*
 * schedule.h: definitions for the ubik scheduler
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

#include "ubik/ubik.h"

struct ubik_scheduler;
struct ubik_exec_unit;

typedef ubik_error (*ubik_exec_notify_func)(
        void *arg,
        struct ubik_scheduler *s,
        struct ubik_exec_unit *e);

struct ubik_exec_notify
{
        /* The function called when a unit of execution has completed */
        ubik_exec_notify_func notify;

        /* The first argument provided to the function when called */
        void *arg;
};

struct ubik_exec_unit
{
        /* The node to be evaluated */
        struct ubik_dagc_node *node;

        /* The graph in which the node exists */
        struct ubik_dagc *graph;

        /* The environment in which to execute the node */
        struct ubik_env *env;

        /* The function to call once execution is complete */
        struct ubik_exec_notify *notify;

        /* The next execution unit in the stack; callers to ubik_schedule_push
         * need not set this field, as the result will be overwritten anyawy. */
        struct ubik_exec_unit *next;
};

/* Creates a scheduler. */
no_ignore ubik_error
ubik_schedule_new(struct ubik_scheduler **s);

/* Destroys a scheduler. */
no_ignore ubik_error
ubik_schedule_free(struct ubik_scheduler *s);

/* Pushes a graph into the scheduler for execution. */
no_ignore ubik_error
ubik_schedule_push(
        struct ubik_scheduler *s,
        struct ubik_dagc *graph,
        struct ubik_env *env,
        struct ubik_exec_notify *notify);

/* Marks an execution unit complete. */
no_ignore ubik_error
ubik_schedule_complete(struct ubik_scheduler *s, struct ubik_exec_unit *e);

/* Runs all queued jobs on the scheduler. */
no_ignore ubik_error
ubik_schedule_run(struct ubik_scheduler *s);

/* Dumps information about what's scheduled to stdout. */
no_ignore ubik_error
ubik_schedule_dump(struct ubik_scheduler *s);
