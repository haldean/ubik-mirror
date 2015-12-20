/*
 * dagc.h: directed acyclic graphs of computation
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

#ifndef EXPEL_DAGC_H
#define EXPEL_DAGC_H

#include "expel/expel.h"
#include "expel/substrate.h"
#include "expel/const.h"

struct xl_dagc_apply {
        struct xl_dagc_node *func;
        struct xl_dagc_node *arg;
};

struct xl_dagc_value {
        struct xl_value *type;
        struct xl_value *value;
};

struct xl_dagc_load {
        struct xl_substrate_id item;
};

struct xl_dagc_store {
        struct xl_substrate_id item;
};

union _xl_dagc_node_details {
        struct xl_dagc_apply apply;
        struct xl_dagc_value value;
        struct xl_dagc_load load;
        struct xl_dagc_store store;
};

struct xl_dagc_node {
        word_t node_type;
        struct xl_value *known_type;
        union _xl_dagc_node_details d;
};

#endif
