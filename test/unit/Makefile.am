include ../../res/build-config.am

TESTS = buffer-test env-test gc-test host_to_net-test load_save-test path_concat-test pointer_set-test split-test uri-test
check_PROGRAMS = buffer-test env-test gc-test host_to_net-test load_save-test path_concat-test pointer_set-test split-test uri-test

buffer_test_SOURCES=buffer.c
env_test_SOURCES=env.c
gc_test_SOURCES=gc.c
host_to_net_test_SOURCES=host_to_net.c
load_save_test_SOURCES=load_save.c
path_concat_test_SOURCES=path_concat.c
pointer_set_test_SOURCES=pointer_set.c
split_test_SOURCES=split.c
uri_test_SOURCES=uri.c

LDADD = $(top_builddir)/libexpel/libexpel.a
