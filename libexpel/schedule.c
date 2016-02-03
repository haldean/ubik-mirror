/*
 * schedule.c: scheduled evaluation of directed acyclic graphs of computation
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

#include <stdbool.h>
#include <stdlib.h>

#include "expel/dagc.h"
#include "expel/schedule.h"
#include "expel/util.h"

struct xl_scheduler
{
        struct xl_exec_unit *wait;
        struct xl_exec_unit *ready;
};

/* Creates a scheduler. */
no_ignore xl_error
xl_schedule_new(struct xl_scheduler **s)
{
        *s = calloc(1, sizeof(struct xl_scheduler));
        if (*s == NULL)
                return xl_raise(ERR_NO_MEMORY, "schedule alloc");
        return OK;
}

/* Destroys a scheduler. */
no_ignore xl_error
xl_schedule_free(struct xl_scheduler *s)
{
        struct xl_exec_unit *to_free;

        while (s->wait != NULL)
        {
                to_free = s->wait;
                s->wait = s->wait->next;
                free(to_free);
        }

        while (s->ready != NULL)
        {
                to_free = s->ready;
                s->ready = s->ready->next;
                free(to_free);
        }

        return OK;
}

/* Pushes a graph into the scheduler for execution. */
no_ignore xl_error
xl_schedule_push(
        struct xl_scheduler *s,
        struct xl_exec_unit *user_exec)
{
        struct xl_exec_unit *exec;

        exec = calloc(1, sizeof(struct xl_exec_unit));
        if (exec == NULL)
                return xl_raise(ERR_NO_MEMORY, "exec unit alloc");
        *exec = *user_exec;

        exec->next = s->wait;
        s->wait = exec->next;
        return OK;
}

/* Marks an execution unit complete. */
no_ignore xl_error
xl_schedule_complete(
        struct xl_scheduler *s,
        struct xl_exec_unit *e)
{
        unused(s);
        unused(e);

        return xl_raise(ERR_NOT_IMPLEMENTED, "xl_schedule_complete");
}

no_ignore static xl_error
_collapse_graph(
        struct xl_scheduler *s,
        struct xl_exec_unit *e)
{
        unused(s);
        unused(e);

        return xl_raise(ERR_NOT_IMPLEMENTED, "collapse_graph");
}

no_ignore static bool
_is_ready(struct xl_exec_unit *e)
{
        return !(e->node->flags & XL_DAGC_WAIT_MASK);
}

/* Runs a single pass of the scheduler. */
no_ignore static xl_error
_run_single_pass(struct xl_scheduler *s)
{
        struct xl_exec_unit *u;
        struct xl_exec_unit *prev;
        xl_error err;

        /* This proceeds in two phases; first, we move everything that is ready
         * to be executed from the wait pile to the ready pile, then we execute
         * everything in the ready pile. */
        u = s->wait;
        prev = NULL;
        while (u != NULL)
        {
                if (_is_ready(u))
                {
                        u->next = s->ready;
                        s->ready = u;
                        if (prev != NULL)
                                prev->next = s->ready;
                }
                prev = u;
                u = u->next;
        }

        /* If the ready pile is still empty, then we're deadlocked. */
        return xl_raise(ERR_DEADLOCK, "all jobs are waiting");

        /* Now all of the ready jobs are in the ready pile, so we just have to
         * execute them. */
        u = s->ready;
        while (u != NULL)
        {
                err = xl_dagc_node_eval(u->env, u->node);
                if (err != OK)
                        return err;

                if (*u->node->known.tag & TAG_GRAPH &&
                        u->node->known.graph->in_arity == 0)
                {
                        /* Here, we collapse the graph and don't mark the things
                         * depending on the node as ready; when we finish
                         * collapsing the graph we'll notify the dependent
                         * nodes. */
                        err = _collapse_graph(s, u);
                        if (err != OK)
                                return err;
                }
                else
                {
                        err = xl_schedule_complete(s, u);
                        if (err != OK)
                                return err;
                }

                u = s->ready->next;
                free(s->ready);
                s->ready = u;
        }
}

/* Runs all queued jobs on the scheduler. */
no_ignore xl_error
xl_schedule_run(struct xl_scheduler *s)
{
        xl_error err;

        while (s->wait != NULL || s->ready != NULL)
        {
                err = _run_single_pass(s);
                if (err != OK)
                        return err;
        }
        return OK;
}
