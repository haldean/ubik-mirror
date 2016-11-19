top = ..
-include ../res/build-config.mk

c_sources = \
    ./codegen/assign.c \
    ./codegen/ast.c \
    ./codegen/closure.c \
    ./codegen/compile.c \
    ./codegen/gen.c \
    ./codegen/grammar.c \
    ./codegen/import.c \
    ./codegen/infer.c \
    ./codegen/interfaces.c \
    ./codegen/literate.c \
    ./codegen/package.c \
    ./codegen/parse.c \
    ./codegen/patterns.c \
    ./codegen/print-expr.c \
    ./codegen/resolve.c \
    ./codegen/token.c \
    ./codegen/types.c \
    ./codegen/typesystem.c \
    ./rt/eval.c \
    ./rt/mem.c \
    ./rt/rat.c \
    ./rt/schedule.c \
    ./rt/str.c \
    ./rt/timer.c \
    ./shared/adt.c \
    ./shared/alloc.c \
    ./shared/assert.c \
    ./shared/bytecode.c \
    ./shared/env.c \
    ./shared/error.c \
    ./shared/explain.c \
    ./shared/feedback.c \
    ./shared/fun.c \
    ./shared/list.c \
    ./shared/natives.c \
    ./shared/pointerset.c \
    ./shared/print-ast.c \
    ./shared/rt.c \
    ./shared/rttypes.c \
    ./shared/stream.c \
    ./shared/streamutil.c \
    ./shared/string.c \
    ./shared/typ.c \
    ./shared/uri.c \
    ./shared/util.c \
    ./shared/value.c \
    ./shared/vector.c \
    ./shared/natives/native-adt.c \
    ./shared/natives/native-arithmetic.c \
    ./shared/natives/native-bool.c \
    ./shared/natives/native-seq.c \
    ./shared/natives/native-util.c

CFLAGS := $(CFLAGS) -Itree -I.
objects = $(patsubst %.c,%.o,$(c_sources))
deps = $(patsubst %.c,%.d,$(c_sources))
built_headers = codegen/grammar.h

libubik.a: $(objects)
	$(AR) rcs $@ $(objects)

$(objects): %.o: %.c $(built_headers)
	$(CC) -c -fPIC -MD -o $@ $(CFLAGS) $<

-include $(patsubst %.o,%.d,$(objects))

codegen/grammar.y: codegen/grammar/*.y
	ls -1 codegen/grammar/*.y | sort | xargs cat > $@

codegen/grammar.c codegen/grammar.h: codegen/grammar.y
	$(BISON) --output=codegen/grammar.c \
		--defines=codegen/grammar.h \
		--report=state -Wall -Werror $<

codegen/token.c: codegen/token.l
	$(FLEX) -o $@ $<

clean:
	rm -f libubik.a \
		$(objects) \
		$(deps) \
		codegen/grammar.{c,h,output,y} \
	       	codegen/token.c

.PHONY: clean
