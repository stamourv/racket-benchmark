ALL_TESTS=$(shell ls *.rkt)

.PHONY: tests
tests:
	@make -k $(foreach t,$(ALL_TESTS), $(t)_test)

.PHONY: clean
clean:
	rm -f *.pdf

%_test: %
	racket $* > /dev/null
