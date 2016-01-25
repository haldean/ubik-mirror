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

#include "expel/expel.h"

struct xl_scheduler;

/* Push a node back into the scheduler.
 *
 * This checks the status of the node and performs the appropriate action; if
 * the node is marked as completed, the nodes waiting on this node are
 * scheduled. If the node is marked as waiting, the nodes that the node is
 * waiting on will be scheduled. */
no_ignore xl_error_t
xl_schedule_push(struct xl_scheduler *s, struct xl_dagc_node *n);
