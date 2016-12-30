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

#pragma once
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

/* node is ready to be evaluated */
#define UBIK_STATUS_READY      0x00
/* node is fully evaluated */
#define UBIK_STATUS_COMPLETE   0x01
/* waiting on node's first dependency */
#define UBIK_STATUS_WAIT_D1    0x02
/* waiting on node's second dependency */
#define UBIK_STATUS_WAIT_D2    0x04
/* waiting on node's third dependency */
#define UBIK_STATUS_WAIT_D3    0x08
/* waiting on evaluation */
#define UBIK_STATUS_WAIT_EVAL  0x10
/* waiting on data to exist */
#define UBIK_STATUS_WAIT_DATA  0x20
/* wait_d1 | wait_d2 | wait_d3 | wait_eval */
#define UBIK_STATUS_WAIT_MASK  0x2E

struct ubik_exec_graph
{
        /* The value being evaluated */
        struct ubik_value *v;

        /* Information about the status of each node. Each element in this array
           is the bitwise union of some number of the UBIK_STATUS constants. */
        uint8_t *status;

        /* The calculated value of each node. */
        struct ubik_value **nv;

        /* The calculated type of each node. */
        struct ubik_value **nt;

        /* The environment in which to execute the node */
        struct ubik_env *env;

        /* The function to call once execution is complete */
        struct ubik_exec_notify *notify;

        /* The workspace in which we're working */
        struct ubik_workspace *workspace;

        /* The number of exec units that refer to this executor */
        uint64_t refcount;

        /* If true, this is executing in a temporary environment that should be
           cleaned up when execution is complete */
        bool transient_env;

        /* Any access to status, nv, nt, or refcount in this struct must be
           done with this lock held. */
        struct ubik_rwlock lock;
};

struct ubik_exec_unit
{
        /* The node to be evaluated */
        ubik_word node;

        /* The graph evaluator for the graph in which the node exists */
        struct ubik_exec_graph *gexec;

        /* The next execution unit in the stack; callers to ubik_schedule_push
           need not set this field, as the result will be overwritten anyawy. */
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
        struct ubik_value *graph,
        struct ubik_env *env,
        bool transient_env,
        struct ubik_exec_notify *notify,
        struct ubik_workspace *workspace);

/* Pushes all root values from a workspace into the scheduler for execution. */
no_ignore ubik_error
ubik_schedule_push_roots(
        struct ubik_scheduler *s,
        struct ubik_env *env,
        struct ubik_workspace *ws);

/* Marks an execution unit complete. */
no_ignore ubik_error
ubik_schedule_complete(struct ubik_scheduler *s, struct ubik_exec_unit *e);

/* Runs all queued jobs on the scheduler. */
no_ignore ubik_error
ubik_schedule_run(struct ubik_scheduler *s);

/* Dumps information about what's scheduled to stdout. */
no_ignore ubik_error
ubik_schedule_dump(struct ubik_scheduler *s);
