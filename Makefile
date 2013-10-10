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
