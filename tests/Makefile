RECORD_GET_BENCH_FILE = record-get.bench

.PHONY: tests
tests: stats-tests test-record-get

.PHONY: stats-tests
stats-tests:
	racket stats.rkt

.PHONY: test-record-get
test-record-get: clean-record-get
	racket record-get.rkt > /dev/null

.PHONY: clean
clean: clean-record-get

.PHONY: clean-record-get
clean-record-get:
	rm -f $(RECORD_GET_BENCH_FILE) tmp-$(RECORD_GET_BENCH_FILE)
