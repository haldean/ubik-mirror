include ../res/build-config.am

lib_LIBRARIES = libexpel.a
libexpel_a_SOURCES = store.c error.c uri.c gen.c timer.c eval.c load.c gc.c pointerset.c compile.c string.c schedule.c natives.c types.c print-ast.c value.c bdagc.c assert.c explain.c ast.c dagc.c parse.c util.c rt.c stream.c env.c token.l grammar.y uri-value.tree humanize-poly.tree
BUILT_SOURCES = grammar.h uri-value.h humanize-poly.h

libexpel_a_CFLAGS = -fPIC

.tree.h:
	$(PYTHON) ../res/buildtree/buildtree.py $< $@
