/*
 * schedule.h: definitions for the expel scheduler
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

struct xl_scheduler;
struct xl_exec_unit;

typedef xl_error (*xl_exec_notify_func)(
        void *arg,
        struct xl_scheduler *s,
        struct xl_exec_unit *e);

struct xl_exec_notify
{
        /* The function called when a unit of execution has completed */
        xl_exec_notify_func notify;

        /* The first argument provided to the function when called */
        void *arg;
};

struct xl_exec_unit
{
        /* The node to be evaluated */
        struct xl_dagc_node *node;

        /* The graph in which the node exists */
        struct xl_dagc *graph;

        /* The environment in which to execute the node */
        struct xl_env *env;

        /* The function to call once execution is complete */
        struct xl_exec_notify *notify;

        /* The next execution unit in the stack; callers to xl_schedule_push
         * need not set this field, as the result will be overwritten anyawy. */
        struct xl_exec_unit *next;
};

/* Creates a scheduler. */
no_ignore xl_error
xl_schedule_new(struct xl_scheduler **s);

/* Destroys a scheduler. */
no_ignore xl_error
xl_schedule_free(struct xl_scheduler *s);

/* Pushes a graph into the scheduler for execution. */
no_ignore xl_error
xl_schedule_push(
        struct xl_scheduler *s,
        struct xl_dagc *graph,
        struct xl_env *env,
        struct xl_exec_notify *notify);

/* Marks an execution unit complete. */
no_ignore xl_error
xl_schedule_complete(struct xl_scheduler *s, struct xl_exec_unit *e);

/* Runs all queued jobs on the scheduler. */
no_ignore xl_error
xl_schedule_run(struct xl_scheduler *s);

/* Dumps information about what's scheduled to stdout. */
no_ignore xl_error
xl_schedule_dump(struct xl_scheduler *s);
