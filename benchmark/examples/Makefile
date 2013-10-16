EXCLUDE=plot.rkt
ALL_TESTS=$(shell ls *.rkt)

.PHONY: tests
tests: optimization-coach
	@make -k $(foreach t,$(ALL_TESTS), $(t)_test)

.PHONY: clean
clean:
	rm -f *.pdf

%_test: %
	racket $* > /dev/null

optimization-coach:
	cd optimization-coach ; racket run.rkt ; racket plot.rkt
