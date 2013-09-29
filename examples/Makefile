.PHONY: tests
tests: fib-regression fib-improvement fib-collatz.rkt_test internal-external.rkt_test

.PHONY: clean
clean:
	rm -f *.bench
	rm -rf plots benches

.PHONY: fib-regression
fib-regression:
	bash check_results.sh fast-fib.rkt slow-fib.rkt "Performance regressions"

.PHONY: fib-improvement
fib-improvement:
	bash check_results.sh slow-fib.rkt fast-fib.rkt "Performance improvements"

%_test:
	racket $* > /dev/null
