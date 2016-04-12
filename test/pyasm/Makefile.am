TESTS = add.xlb cond.xlb const.xlb emit-poly.xlb huge.xlb loadstore.xlb mult.xlb please-leak.xlb ref.xlb simplefunc.xlb
XFAIL_TESTS = mult.xlb
TEST_EXTENSIONS = .xlb
XLB_LOG_COMPILER = $(top_srcdir)/ubik/ubik

.xlpy.xlb:
	PYTHONPATH=$(top_srcdir) $(PYTHON) $< $@
