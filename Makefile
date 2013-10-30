SOURCE_DIR=benchmark
IGNORE=benchmark/external optimization-coach/oc-external benchmark/external
SOURCE_FILES=$(shell find $(SOURCE_DIR) -name '*.rkt' -o -name '*.scrbl' | \
				     grep -v "$(shell echo '$(IGNORE)' | sed 's/ /\\|/g')")

.PHONY: tests
tests:
	make -C examples tests
	make -C tests tests

.PHONY: clean
clean:
	rm -rf planet-docs
	make -C examples clean

.PHONY: docs
docs:
	raco setup
	raco doc benchmark

%_echo:
	@echo $($*)
