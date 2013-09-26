.PHONY: tests
tests: fib-regression fib-improvement

.PHONY: clean
clean:
	rm -f *.bench

.PHONY: fib-regression
fib-regression:
	bash check_results.sh fast-fib.rkt slow-fib.rkt "Performance regressions"

.PHONY: fib-improvement
fib-improvement:
	bash check_results.sh slow-fib.rkt fast-fib.rkt "Performance improvements"

