boot_tests = \
  ok-boot-test-0 \
  ok-boot-test-1 \
  ok-boot-test-circular-1 \
  ok-boot-test-circular-2 \
  ok-boot-test-dependency-included \
  ok-boot-test-duplicate-included \
  ok-boot-test-included-deps \
  ok-boot-test-nested-include

ok-boot-test-%: boot-test-%
	./test-boot.pl $<
	@ln -sf `which true` $@

TESTS = 		\
  $(boot_tests)		\
  test-start		\
  test-stop		\
  test-downgrade	\
  test-upgrade

CLEANFILES =		\
  $(boot_tests)

MAINTAINERCLEANFILES = \
  Makefile.in

EXTRA_DIST =		\
  erlrc_boot_test.erl	\
  erlrctest.app		\
  erlrctest.erl		\
  test-downgrade	\
  test-start		\
  test-stop		\
  test-upgrade

maintainer-clean-local:
	rm -f *.out
	rm -f *.log
