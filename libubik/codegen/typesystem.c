/*
 * typesystem.c: maintains type information during compiliation
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

#include "ubik/assert.h"
#include "ubik/compile.h"
#include "ubik/feedback.h"
#include "ubik/string.h"
#include "ubik/typ.h"
#include "ubik/typesystem.h"
#include "ubik/vector.h"

#include <string.h>

enum ts_value_type
{
        TS_TYPE = 1,
        TS_IFACE,
        TS_IMPL,
};

struct ts_type
{
        char *name;
        char *package;
        struct ubik_value *v;
};

struct ts_iface
{
        char *name;
        char *package;
        size_t n_params;
};

struct ts_impl
{
        struct ts_iface *iface;
        struct ubik_type_expr params[UBIK_MAX_INTERFACE_PARAMS];
};

struct ts_value
{
        union
        {
                struct ts_type t;
                struct ts_iface f;
                struct ts_impl m;
        };
        enum ts_value_type vt;
};

struct ubik_typesystem
{
        struct ubik_vector types;
        struct ubik_vector interfaces;
        struct ubik_vector implementations;
        struct ubik_alloc_region *region;
};

no_ignore ubik_error
ubik_typesystem_init(
        struct ubik_typesystem **tsys,
        struct ubik_alloc_region *region,
        struct ubik_workspace *ws)
{
        struct ts_type *ts;
        struct ubik_value *t;
        ubik_error err;

        ubik_alloc1(tsys, struct ubik_typesystem, region);
        (*tsys)->types.region = region;
        (*tsys)->interfaces.region = region;
        (*tsys)->implementations.region = region;
        (*tsys)->region = region;

        err = ubik_value_new(&t, ws);
        if (err != OK)
                return err;
        t->type = UBIK_TYP;
        t->typ.t = UBIK_TYPE_STR;

        ubik_alloc1(&ts, struct ts_type, region);
        ts->v = t;
        ts->name = ubik_strdup("String", region);
        ts->package = "ubik";
        err = ubik_vector_append(&(*tsys)->types, ts);
        if (err != OK)
                return err;

        err = ubik_value_new(&t, ws);
        if (err != OK)
                return err;
        t->type = UBIK_TYP;
        t->typ.t = UBIK_TYPE_RAT;

        ubik_alloc1(&ts, struct ts_type, region);
        ts->v = t;
        ts->name = ubik_strdup("Number", region);
        ts->package = "ubik";
        err = ubik_vector_append(&(*tsys)->types, ts);
        if (err != OK)
                return err;

        return OK;
}

no_ignore ubik_error
ubik_typesystem_load(
        struct ubik_typesystem *tsys,
        struct ubik_ast *ast,
        struct ubik_compile_request *req)
{
        struct ubik_type *t;
        struct ubik_ast_interface *iface;
        struct ubik_ast_implementation *impl;
        struct ubik_type_params *params;
        struct ubik_type_list *type_list;

        struct ts_type *tst;
        struct ts_iface *tsif;
        struct ts_impl *tsim;

        char *check_pkg;
        size_t i;
        size_t j;
        size_t n_params;
        ubik_error err;

        for (i = 0; i < ast->types.n; i++)
        {
                t = (struct ubik_type *) ast->types.elems[i];
                ubik_alloc1(&tst, struct ts_type, tsys->region);
                tst->name = ubik_strdup(t->name, tsys->region);
                tst->package = ubik_strdup(ast->package_name, tsys->region);
                /* add to list first, to enable recursive type definitions */
                err = ubik_vector_append(&tsys->types, tst);
                if (err != OK)
                        return err;
                err = ubik_typ_from_ast(&tst->v, t, tsys, req->workspace);
                if (err != OK)
                        return err;
        }

        for (i = 0; i < ast->interfaces.n; i++)
        {
                iface = (struct ubik_ast_interface *) ast->interfaces.elems[i];
                ubik_alloc1(&tsif, struct ts_iface, tsys->region);
                tsif->name = ubik_strdup(iface->name, tsys->region);
                tsif->package = ubik_strdup(ast->package_name, tsys->region);
                for (params = iface->params, tsif->n_params = 0;
                        params != NULL;
                        params = params->next, tsif->n_params++);

                if (tsif->n_params > UBIK_MAX_INTERFACE_PARAMS)
                {
                        ubik_feedback_error_line(
                                req->feedback,
                                UBIK_FEEDBACK_ERR,
                                &iface->loc,
                                "interfaces cannot have more than %d "
                                "parameters, %s has %lu",
                                UBIK_MAX_INTERFACE_PARAMS,
                                iface->name,
                                tsif->n_params);
                        return ubik_raise(
                                ERR_UNKNOWN_TYPE, "too many interface params");
                }

                err = ubik_vector_append(&tsys->interfaces, tsif);
                if (err != OK)
                        return err;
        }

        for (i = 0; i < ast->implementations.n; i++)
        {
                impl = (struct ubik_ast_implementation *)
                        ast->implementations.elems[i];
                ubik_alloc1(&tsim, struct ts_impl, tsys->region);
                tsim->iface = NULL;
                check_pkg = impl->iface_package == NULL ?
                        ast->package_name : impl->iface_package;

                for (j = 0; j < tsys->interfaces.n; j++)
                {
                        tsif = tsys->interfaces.elems[j];
                        if (ubik_strcmp(tsif->name, impl->iface_name) != 0)
                                continue;
                        if (ubik_strcmp(tsif->package, check_pkg) != 0)
                                continue;
                        tsim->iface = tsif;
                        break;
                }
                if (tsim->iface == NULL && impl->iface_package == NULL)
                {
                        ubik_feedback_error_line(
                                req->feedback,
                                UBIK_FEEDBACK_ERR,
                                &impl->loc,
                                "implementation of unknown interface %s",
                                impl->iface_name);
                        return ubik_raise(
                                ERR_UNKNOWN_TYPE, "impl of unknown interface");
                }

                for (n_params = 0, type_list = impl->params;
                        type_list != NULL;
                        type_list = type_list->next, n_params++);
                if (n_params != tsim->iface->n_params)
                {
                        ubik_feedback_error_line(
                                req->feedback,
                                UBIK_FEEDBACK_ERR,
                                &impl->loc,
                                "too many interface parameters given, "
                                "interface %s has %lu parameters, but "
                                "implementation has %lu",
                                tsim->iface->name,
                                tsim->iface->n_params,
                                n_params);
                        return ubik_raise(
                                ERR_UNKNOWN_TYPE, "impl of unknown interface");
                }

                for (j = 0, type_list = impl->params;
                        type_list != NULL;
                        type_list = type_list->next, j++)
                {
                        err = ubik_type_expr_copy(
                                &tsim->params[j],
                                type_list->type_expr,
                                tsys->region);
                        if (err != OK)
                                return err;
                }

                err = ubik_vector_append(&tsys->implementations, tsim);
                if (err != OK)
                        return err;
        }

        return OK;
}

void
ubik_typesystem_dump(struct ubik_typesystem *tsys)
{
        size_t i;
        size_t j;
        struct ts_type *type;
        struct ts_iface *iface;
        struct ts_impl *impl;

        printf("\n%lu known types:\n", tsys->types.n);
        for (i = 0; i < tsys->types.n; i++)
        {
                type = tsys->types.elems[i];
                printf("\t%s:%s\n", type->package, type->name);
        }
        printf("%lu known interfaces:\n", tsys->interfaces.n);
        for (i = 0; i < tsys->interfaces.n; i++)
        {
                iface = tsys->interfaces.elems[i];
                printf("\t%s:%s %lu\n",
                        iface->package, iface->name, iface->n_params);
        }
        printf("%lu known implementations:\n", tsys->implementations.n);
        for (i = 0; i < tsys->implementations.n; i++)
        {
                impl = tsys->implementations.elems[i];
                printf("\t%s:%s", impl->iface->package, impl->iface->name);
                for (j = 0; j < impl->iface->n_params; j++)
                {
                        printf(" ");
                        ubik_assert(
                                ubik_type_expr_print(&impl->params[j]) == OK);
                }
                printf("\n");
        }
}

no_ignore static ubik_error
assign_atom_to_atom(
        struct ubik_typesystem_unified *unified,
        struct ubik_type_expr *to,
        struct ubik_type_expr *from)
{
        if (strcmp(to->name, from->name) != 0)
        {
                unified->success = false;
        }
        return OK;
}

/* Resolves two attempted substitutions for the same variable. Returns the
 * result by modifying res. If the substitution is impossible, res is unmodified
 * and the success flag in unified is set to false. */
no_ignore static ubik_error
resolve_substs(
        struct ubik_typesystem_subst *res,
        struct ubik_typesystem_unified *unified,
        struct ubik_typesystem *tsys,
        struct ubik_type_expr *expr1,
        struct ubik_type_expr *expr2,
        struct ubik_alloc_region *region)
{
        struct ubik_typesystem_unified u = {0};
        ubik_error err;

        err = ubik_typesystem_unify(
                &u, tsys, NULL, expr1, expr2, region, false);
        if (err != OK)
                return err;
        if (u.success == false)
        {
                unified->success = false;
                unified->failure_info = u.failure_info;
                return OK;
        }
        res->val = u.res;
        return OK;
}

no_ignore static ubik_error
assign_to_var(
        struct ubik_typesystem_unified *unified,
        struct ubik_typesystem *tsys,
        struct ubik_type_expr *var,
        struct ubik_type_expr *expr,
        struct ubik_alloc_region *region)
{
        struct ubik_typesystem_subst *sub;
        size_t i;

        for (i = 0; i < unified->substs.n; i++)
        {
                sub = unified->substs.elems[i];
                if (strcmp(sub->varname, var->name) == 0)
                        return resolve_substs(
                                sub, unified, tsys, expr, sub->val, region);
        }
        ubik_alloc1(&sub, struct ubik_typesystem_subst, region);
        sub->varname = var->name;
        sub->val = expr;
        return ubik_vector_append(&unified->substs, sub);
}

no_ignore static ubik_error
unify(
        struct ubik_typesystem_unified *unified,
        struct ubik_typesystem *tsys,
        char *package,
        struct ubik_type_expr *assign_to,
        struct ubik_type_expr *assign_from,
        struct ubik_alloc_region *region)
{
        ubik_error err;

        switch (assign_to->type_expr_type)
        {
        case TYPE_EXPR_ATOM:
                if (assign_from->type_expr_type == TYPE_EXPR_VAR)
                        return assign_to_var(
                                unified, tsys, assign_from, assign_to, region);
                if (assign_from->type_expr_type == TYPE_EXPR_ATOM)
                        return assign_atom_to_atom(
                                unified, assign_to, assign_from);
                unified->success = false;
                return OK;

        case TYPE_EXPR_CONSTRAINED:
                unified->success = false;
                return ubik_raise(
                        ERR_BAD_TYPE,
                        "constrained types should have been stripped out");

        case TYPE_EXPR_APPLY:
                if (assign_from->type_expr_type != TYPE_EXPR_APPLY)
                {
                        printf("unsupported comparison between applied types\n");
                        return OK;
                }
                err = unify(
                        unified, tsys, package, assign_to->apply.head,
                        assign_from->apply.head, region);
                if (err != OK || !unified->success)
                        return err;
                err = unify(
                        unified, tsys, package, assign_to->apply.tail,
                        assign_from->apply.tail, region);
                return err;

        case TYPE_EXPR_VAR:
                return assign_to_var(
                        unified, tsys, assign_to, assign_from, region);
        }

        return ubik_raise(ERR_BAD_TYPE, "bad type expr type in unify");
}

no_ignore static ubik_error
add_constraints(
        struct ubik_typesystem *tsys,
        struct ubik_vector *cs,
        struct ubik_type_constraints *expr_cs,
        struct ubik_alloc_region *region)
{
        struct ts_iface *iface;
        struct ts_impl *impl;
        struct ubik_type_params *params;
        ubik_error err;
        bool found;
        size_t i;

        while (expr_cs != NULL)
        {
                found = false;
                for (i = 0; i < tsys->interfaces.n; i++)
                {
                        iface = tsys->interfaces.elems[i];
                        /* TODO: actual package handling here. */
                        if (strcmp(expr_cs->interface, iface->name) == 0)
                        {
                                found = true;
                                break;
                        }
                }
                if (!found)
                        return ubik_raise(
                                ERR_BAD_TYPE, "unknown interface in constraint");

                ubik_alloc1(&impl, struct ts_impl, region);
                impl->iface = iface;
                params = expr_cs->params;

                for (params = expr_cs->params, i = 0;
                     params != NULL && i < iface->n_params;
                     params = params->next, i++)
                {
                        impl->params[i].name = ubik_strdup(params->name, region);
                        impl->params[i].type_expr_type = TYPE_EXPR_VAR;
                }
                if (params != NULL || i != iface->n_params)
                        return ubik_raise(
                                ERR_BAD_TYPE,
                                "wrong number of params in constraint");

                err = ubik_vector_append(cs, impl);
                if (err != OK)
                        return err;
                expr_cs = expr_cs->next;
        }
        return OK;
}

no_ignore static ubik_error
apply_subst(
        struct ubik_type_expr *expr,
        struct ubik_typesystem_subst *sub)
{
        ubik_error err;

        switch (expr->type_expr_type)
        {
        case TYPE_EXPR_ATOM:
                return OK;

        case TYPE_EXPR_CONSTRAINED:
                return ubik_raise(
                        ERR_BAD_TYPE,
                        "constrained types should have been stripped out");

        case TYPE_EXPR_APPLY:
                err = apply_subst(expr->apply.head, sub);
                if (err != OK)
                        return err;
                err = apply_subst(expr->apply.tail, sub);
                return err;

        case TYPE_EXPR_VAR:
                if (strcmp(expr->name, sub->varname) != 0)
                        return OK;
                *expr = *(sub->val);
                return OK;
        }

        return ubik_raise(ERR_BAD_TYPE, "bad type expr type in unify");
}

no_ignore static ubik_error
replace_var_in_impl(
        struct ts_impl *constraint,
        struct ubik_typesystem_subst *sub)
{
        size_t i;
        ubik_error err;
        for (i = 0; i < constraint->iface->n_params; i++)
        {
                err = apply_subst(&constraint->params[i], sub);
                if (err != OK)
                        return err;
        }
        return OK;
}

no_ignore ubik_error
ubik_typesystem_apply_substs(
        struct ubik_type_expr *t,
        struct ubik_vector *substs)
{
        struct ubik_typesystem_subst *sub;
        ubik_error err;
        size_t i;
        for (i = 0; i < substs->n; i++)
        {
                sub = (struct ubik_typesystem_subst *) substs->elems[i];
                err = apply_subst(t, sub);
                if (err != OK)
                        return err;
        }
        return OK;
}

static bool
impl_entailed(
        struct ubik_typesystem *tsys,
        struct ts_impl *constraint)
{
        struct ts_impl *impl;
        struct ubik_type_expr *cp;
        struct ubik_type_expr *ip;
        size_t i, j;
        bool match;

        for (i = 0; i < tsys->implementations.n; i++)
        {
                impl = tsys->implementations.elems[i];
                /* Note to self (because I can just see this causing a bug
                 * later): this is assuming there's a single struct in memory for
                 * each interface. */
                if (impl->iface != constraint->iface)
                        continue;

                match = true;
                for (j = 0; j < impl->iface->n_params; j++)
                {
                        cp = &constraint->params[j];
                        ip = &impl->params[j];
                        /* variables in the constraint match anything */
                        if (cp->type_expr_type == TYPE_EXPR_VAR)
                                continue;
                        if (strcmp(ip->name, cp->name) != 0)
                        {
                                match = false;
                                break;
                        }
                }
                if (match)
                        return true;
        }
        return false;
}

no_ignore ubik_error
ubik_typesystem_unify(
        struct ubik_typesystem_unified *unified,
        struct ubik_typesystem *tsys,
        char *package,
        struct ubik_type_expr *assign_to,
        struct ubik_type_expr *assign_from,
        struct ubik_alloc_region *region,
        bool debug)
{
        struct ubik_typesystem_subst *sub;
        struct ubik_vector constraints = {0};
        struct ts_impl *constraint;
        ubik_error err;
        size_t i, j;

        ubik_assert(tsys != NULL);

        constraints.region = region;

        unified->substs = (struct ubik_vector) {0};
        unified->substs.region = region;

        unified->success = true;
        unified->res = NULL;
        unified->failure_info = NULL;

        if (assign_to->type_expr_type == TYPE_EXPR_CONSTRAINED)
        {
                err = add_constraints(
                        tsys, &constraints, assign_to->constrained.constraints,
                        region);
                if (err != OK)
                        return err;
                assign_to = assign_to->constrained.term;
        }
        if (assign_from->type_expr_type == TYPE_EXPR_CONSTRAINED)
        {
                err = add_constraints(
                        tsys, &constraints, assign_from->constrained.constraints,
                        region);
                if (err != OK)
                        return err;
                assign_from = assign_from->constrained.term;
        }

        err = unify(unified, tsys, package, assign_to, assign_from, region);

        ubik_alloc1(&unified->res, struct ubik_type_expr, region);
        err = ubik_type_expr_copy(unified->res, assign_to, region);
        if (err != OK)
                return err;

        /* Check that constraints are satisfied by applying the substitution to
         * each constraint, and then making sure that implementations exist
         * for each. */
        for (i = 0; i < unified->substs.n; i++)
        {
                sub = unified->substs.elems[i];
                for (j = 0; j < constraints.n; j++)
                {
                        constraint = constraints.elems[j];
                        err = replace_var_in_impl(constraint, sub);
                        if (err != OK)
                                return err;
                }
                err = apply_subst(unified->res, sub);
                if (err != OK)
                        return err;
        }

        for (i = 0; i < constraints.n; i++)
        {
                constraint = constraints.elems[i];
                if (!impl_entailed(tsys, constraint))
                {
                        unified->success = false;
                        ubik_asprintf(
                                &unified->failure_info,
                                region,
                                "could not find an implementation for \x1b[32m%s",
                                constraint->iface->name);
                        for (j = 0; j < constraint->iface->n_params; j++)
                        {
                                ubik_asprintf(
                                        &unified->failure_info,
                                        region,
                                        "%s %s",
                                        unified->failure_info,
                                        constraint->params[j].name);
                        }
                        ubik_asprintf(
                                &unified->failure_info,
                                region,
                                "%s\x1b[0m",
                                unified->failure_info);
                }
        }

        if (debug)
        {
                printf("    unifying ");
                ubik_assert(ubik_type_expr_print(assign_to) == OK);
                printf(" and ");
                ubik_assert(ubik_type_expr_print(assign_from) == OK);
                printf(" gives subst ");
                for (i = 0; i < unified->substs.n; i++)
                {
                        sub = unified->substs.elems[i];
                        printf("(%s => ", sub->varname);
                        ubik_assert(ubik_type_expr_print(sub->val) == OK);
                        printf(") ");
                }
                for (i = 0; i < constraints.n; i++)
                {
                        constraint = constraints.elems[i];
                        printf("requiring impl %s ", constraint->iface->name);
                        for (j = 0; j < constraint->iface->n_params; j++)
                        {
                                printf("%s ", constraint->params[j].name);
                        }
                }
                printf(", result=%d\n", unified->success);
        }

        return err;
}

no_ignore ubik_error
ubik_typesystem_get(
        struct ubik_value **res,
        struct ubik_typesystem *tsys,
        char *name,
        char *package)
{
        size_t i;
        struct ts_type *t;

        for (i = 0; i < tsys->types.n; i++)
        {
                t = tsys->types.elems[i];
                if (strcmp(t->name, name) != 0)
                        continue;
                if (package != NULL && strcmp(t->package, package) != 0)
                        continue;
                *res = t->v;
                return OK;
        }
        printf("failed to find type %s:%s\n", package, name);
        return ubik_raise(
                ERR_ABSENT, "type not found in typesystem");
}

no_ignore ubik_error
ubik_typesystem_get_from_expr(
        struct ubik_value **res,
        struct ubik_typesystem *tsys,
        struct ubik_type_expr *t,
        struct ubik_workspace *ws)
{
        unused(ws);

        switch (t->type_expr_type)
        {
        case TYPE_EXPR_ATOM:
                /* TODO: should have package! */
                return ubik_typesystem_get(res, tsys, t->name, NULL);

        case TYPE_EXPR_VAR:
                return ubik_raise(
                        ERR_BAD_TYPE, "cannot get variable from typesystem");

        case TYPE_EXPR_APPLY:
                return ubik_typesystem_get_from_expr(
                        res, tsys, t->apply.head, ws);

        case TYPE_EXPR_CONSTRAINED:
                return ubik_raise(
                        ERR_NOT_IMPLEMENTED, "complicated type expr lookup");
        }
        return ubik_raise(ERR_BAD_TYPE, "unknown type expr type");
}
