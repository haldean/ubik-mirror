nobase_include_HEADERS = \
	ubik/adt.h \
	ubik/assert.h \
	ubik/assign.h \
	ubik/ast.h \
	ubik/bdagc.h \
	ubik/closure.h \
	ubik/compile.h \
	ubik/const.h \
	ubik/dagc.h \
	ubik/def-native.h \
	ubik/env.h \
	ubik/gc.h \
	ubik/gen.h \
	ubik/infer.h \
	ubik/list.h \
	ubik/natives.h \
	ubik/parse.h \
	ubik/patterns.h \
	ubik/pointerset.h \
	ubik/resolve.h \
	ubik/schedule.h \
	ubik/stream.h \
	ubik/streamutil.h \
	ubik/string.h \
	ubik/timer.h \
	ubik/token.h \
	ubik/types.h \
	ubik/ubik.h \
	ubik/uri.h \
	ubik/util.h \
	ubik/value.h \
	ubik/vector.h

ubik/const.h: ../res/const.txt
	$(AWK) -f ../res/compile-const.awk $< > $@
