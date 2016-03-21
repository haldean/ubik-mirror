TESTS = add.xlb cond.xlb const.xlb emit-poly.xlb huge.xlb loadstore.xlb mult.xlb please-leak.xlb ref.xlb simplefunc.xlb
TEST_EXTENSIONS = .xlb
XLB_LOG_COMPILER = $(top_srcdir)/runexpel/runexpel

.xlpy.xlb:
	PYTHONPATH=$(top_srcdir) $(PYTHON) $< $@
