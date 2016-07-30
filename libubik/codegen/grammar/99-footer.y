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

%%

void
yyerror(
        YYLTYPE *loc,
        struct ubik_parse_context *ctx,
        void *scanner,
        const char *err)
{
        YYLTYPE yyloc;
        unused(scanner);

        ubik_alloc1(&ctx->err_loc, struct ubik_ast_loc, ctx->region);
        yyloc = *loc;
        load_loc(*ctx->err_loc);
        ctx->err_msg = ubik_strdup(err, ctx->region);
}

