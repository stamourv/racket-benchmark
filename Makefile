.PHONY: tests
tests:
	make -C examples tests
	make -C tests tests

.PHONY: clean
clean:
	make -C examples clean
	make -C tests clean
