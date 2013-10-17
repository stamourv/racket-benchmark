EXCLUDE=plot.rkt
ALL_TESTS=$(shell ls *.rkt)

.PHONY: tests
tests: optimization-coach $(foreach t,$(ALL_TESTS), $(t)_test)

.PHONY: clean
clean:
	rm -f *.pdf

%_test: %
	racket $* > /dev/null
	@echo "DONE RUNNING $*"

.PHONY: optimization-coach
optimization-coach:
	cd optimization-coach ; racket run.rkt > /dev/null ; racket plot.rkt > /dev/null
	@echo "DONE RUNNING optimization-coach"
