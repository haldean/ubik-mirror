lib_LIBRARIES = libexpel.a
libexpel_a_SOURCES = store.c error.c uri.c gen.c timer.c eval.c load.c gc.c pointerset.c compile.c string.c schedule.c natives.c types.c print-ast.c value.c bdagc.c assert.c explain.c ast.c dagc.c parse.c util.c rt.c stream.c env.c token.l grammar.y

BUILT_SOURCES = grammar.h

AM_CPPFLAGS = -I../include/ -D_GNU_SOURCE -D_FORTIFY_SOURCE=2
AM_YFLAGS = -d --report=state -Wall -Werror

