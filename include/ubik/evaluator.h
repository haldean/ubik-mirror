/*
 * evaluator.h: evalutes functions in a queue
 * Copyright (C) 2017, Haldean Brown
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
#include "ubik/ubik.h"

struct ubik_evaluator;
struct ubik_evaluate_callback;

typedef ubik_error (*ubik_evaluator_cbfunc)(
        struct ubik_evaluate_callback *data,
        struct ubik_value *v,
        struct ubik_value *t,
        struct ubik_value **all_values);

struct ubik_evaluate_callback
{
        ubik_evaluator_cbfunc func;
};

no_ignore ubik_error
ubik_evaluate_new(
        struct ubik_evaluator **evaluator,
        struct ubik_env *env,
        struct ubik_workspace *ws);

void
ubik_evaluate_free(struct ubik_evaluator *evaluator);

no_ignore ubik_error
ubik_evaluate_push(
        struct ubik_evaluator *evaluator,
        struct ubik_value *v,
        struct ubik_evaluate_callback *cb);

no_ignore ubik_error
ubik_evaluate_push_roots(
        struct ubik_evaluator *evaluator,
        struct ubik_workspace *ws);

no_ignore ubik_error
ubik_evaluate_run(struct ubik_evaluator *evaluator);
