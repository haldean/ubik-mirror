TESTS = \
	add.ub \
	cond.ub \
	const.ub \
	emit-poly.ub \
	huge.ub \
	loadstore.ub \
	mult.ub \
	please-leak.ub \
	ref.ub \
	simplefunc.ub

XFAIL_TESTS = mult.ub
TEST_EXTENSIONS = .ub
UB_LOG_COMPILER = $(top_srcdir)/bin/ubik

.upy.ub:
	PYTHONPATH=$(top_srcdir) $(PYTHON) $< $@
