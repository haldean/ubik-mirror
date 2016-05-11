lib_LIBRARIES = libubik.a
libubik_a_SOURCES = store.c error.c uri.c gen.c timer.c eval.c load.c gc.c pointerset.c compile.c string.c schedule.c natives.c types.c print-ast.c value.c bdagc.c assert.c explain.c ast.c dagc.c parse.c util.c rt.c stream.c env.c token.l grammar.y uri-value.tree humanize-poly.tree vector.c resolve.c closure.c streamutil.c infer.c assign.c
BUILT_SOURCES = grammar.h uri-value.h humanize-poly.h

libubik_a_CFLAGS = -std=c11 -Werror -Wall -Wextra -fno-strict-aliasing -rdynamic -Wswitch-enum -fPIC
libubik_a_CPPFLAGS = -I$(top_srcdir)/include/ -D_GNU_SOURCE #-D_FORTIFY_SOURCE=2
AM_YFLAGS = -d --report=state -Wall #-Werror

.tree.h:
	$(PYTHON) ../res/buildtree/buildtree.py $< $@
