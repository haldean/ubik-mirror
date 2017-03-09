/* vim: set shiftwidth=8 tabstop=8 filetype=text :
 * grammar.y: ubik language grammar
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

interface_def
: INTERFACE TYPE_NAME type_params interface_members
{
        alloc($$, 1, struct ubik_ast_interface);
        $$->name.name = $2;
        $$->params = $3;
        $$->members = $4;
        load_loc($$->loc);
        merge_loc($$, $$, $4);
}
;

interface_member
: MEMBER NAME TYPE top_type_expr
{
        alloc($$, 1, struct ubik_ast_member_list);
        $$->name = $2;
        $$->type = $4;
        $$->value = NULL;
        $$->next = NULL;
        load_loc($$->loc);
        merge_loc($$, $$, $4);
}
;

interface_members
: interface_member interface_members
{
        $$ = $1;
        $$->next = $2;
}
| interface_member
;

impl_def
: DEFINES TYPE_NAME type_list impl_members
{
        alloc($$, 1, struct ubik_ast_implementation);
        $$->iface_name = $2;
        $$->params = $3;
        $$->members = $4;
        load_loc($$->loc);
        merge_loc($$, $$, $4);
}
| DEFINES QUALIFIED_TYPE_NAME type_list impl_members
{
        alloc($$, 1, struct ubik_ast_implementation);
        wrap_err(ubik_ast_read_qualified(
                &$$->iface_package, &$$->iface_name, $2, ctx->region));
        $$->params = $3;
        $$->members = $4;
        load_loc($$->loc);
        merge_loc($$, $$, $4);
}
;

impl_member
: MEMBER NAME IS top_expr
{
        alloc($$, 1, struct ubik_ast_member_list);
        $$->name = $2;
        $$->type = NULL;
        $$->value = $4;
        $$->next = NULL;
        load_loc($$->loc);
        merge_loc($$, $$, $4);
}
;

impl_members
: impl_member impl_members
{
        $$ = $1;
        $$->next = $2;
}
| impl_member
;
