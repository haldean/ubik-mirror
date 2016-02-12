/*
 * ast.h: in-memory ast representation
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


#include "expel/expel.h"

struct xl_ast_binding
{
        wchar_t *name;
        union xl_atom value;
};

struct xl_ast
{
        struct xl_ast_binding *bindings;
        size_t n_bindings;
};

/* Allocates a new AST. */
no_ignore xl_error
xl_ast_new(struct xl_ast **ast);

no_ignore xl_error
xl_ast_free(struct xl_ast *ast);

no_ignore xl_error
xl_ast_bind(struct xl_ast *ast, struct xl_ast_binding *bind);

no_ignore xl_error
xl_ast_binding_new(struct xl_ast_binding **binding, wchar_t *name);
