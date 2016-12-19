/* vim: set shiftwidth=8 tabstop=8 :
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

package
: DEFINES NAME
{
        $$ = $2;
}
;

import
: IMPORT NAME
{
        alloc($$, 1, struct ubik_ast_import_list);
        $$->canonical = $2;
        $$->name = ubik_strdup($2, ctx->region);
        load_loc($$->loc);
}
| IMPORT NAME IMPLIES NAME
{
        alloc($$, 1, struct ubik_ast_import_list);
        $$->canonical = $2;
        $$->name = $4;
        load_loc($$->loc);
}
| IMPORT SPLAT NAME
{
        alloc($$, 1, struct ubik_ast_import_list);
        $$->canonical = $3;
        $$->name = ubik_strdup("", ctx->region);
        load_loc($$->loc);
}
;


