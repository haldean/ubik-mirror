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

test_expr
: TEST expr IS expr
{
        alloc($$, 1, struct ubik_ast_test);
        $$->actual = $2;
        $$->expected = $4;
        load_loc($$->loc);
        merge_loc($$, $$, $2);
        merge_loc($$, $$, $4);
}
;
