SOURCE_DIR=benchmark
IGNORE=examples/external optimization-coach/oc-external \#
SOURCE_FILES=$(shell find $(SOURCE_DIR) -name '*.rkt' -o -name '*.scrbl' | \
				grep -v "$(shell echo '$(IGNORE)' | sed 's/ /\\|/g')")

NUM_JOBS?=8

link_install:
	raco pkg install --deps force -n benchmark --link .

.PHONY: tests
tests:
	make -k -j $(NUM_JOBS) -C benchmark/examples tests

.PHONY: clean
clean:
	@find . -type d -name compiled -exec rm -rf {} \; || true
	@make -C benchmark/examples clean

%_echo:
	@echo $($*)
