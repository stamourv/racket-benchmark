SOURCE_DIR=benchmark
IGNORE=examples/external optimization-coach/oc-external \#
SOURCE_FILES=$(shell find $(SOURCE_DIR) -name '*.rkt' -o -name '*.scrbl' | \
				grep -v "$(shell echo '$(IGNORE)' | sed 's/ /\\|/g')")

5.3.6_DEPS=base $(HEAD_DEPS)
HEAD_DEPS=math-lib plot-gui-lib plot-lib srfi-lite-lib typed-racket-lib

NUM_JOBS?=8

%_link_install: %_deps
	raco pkg install -n benchmark --link .

%_deps:
	raco pkg install $($*_DEPS)

.PHONY: tests
tests:
	make -k -j $(NUM_JOBS) -C benchmark/examples tests

.PHONY: clean
clean:
	@find . -type d -name compiled -exec rm -rf {} \; || true
	@make -C benchmark/examples clean

%_echo:
	@echo $($*)
