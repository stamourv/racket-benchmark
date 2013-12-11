IGNORE=sleep.rkt
ALL_TESTS=$(shell ls *.rkt | \
			grep -v "$(shell echo '$(IGNORE)' | sed 's/ /\\|/g')")

.PHONY: tests
tests: $(foreach t,$(ALL_TESTS), $(t)_test)

.PHONY: clean
clean:
	rm -f *.pdf

%_test: %
	raco make $* > /dev/null
	racket $* > /dev/null
	@echo "DONE RUNNING $*"

.PHONY: optimization-coach
optimization-coach:
	cd optimization-coach ; racket run.rkt > /dev/null
	cd optimization-coach ; racket plot.rkt > /dev/null
	@echo "DONE RUNNING optimization-coach"

.PHONY: sanity
sanity:
	cd sanity ; racket sanity-run.rkt  > /dev/null
	cd sanity ; racket sanity-plot.rkt > /dev/null
	@echo "DONE RUNNING sanity"

.PHONY: all
all: tests optimization-coach sanity

.PHONY: clean
clean:
	rm -f *~
	rm -f *.bench-*
	rm -f results-info-*
	find . -type d -name compiled -exec rm -rf {} \; || true
