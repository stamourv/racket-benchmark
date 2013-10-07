ALL_TESTS=fib-collatz.rkt internal-external.rkt fast-fib.rkt internal-external.rkt jit-no-jit.rkt plot.rkt slow-fib.rkt

.PHONY: tests
tests:
	@make -k $(foreach t,$(ALL_TESTS), $(t)_test)

.PHONY: clean
clean:
	rm -rf plots benches

%_test: %
	racket $* > /dev/null
