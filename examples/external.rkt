#lang racket

(require rackunit)
(require "../src/benchmark.rkt")
(require "../src/types.rkt")

;; benchmark tests
(define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))

(define (fib-internal n)
  (fib n)
  (time-one (lambda () (for ([i 100]) (fib n))))
  (fib n))

(define fib-internal-group
  (mk-benchmark-group
   "internals"
   (list (mk-benchmark-one "fib-internal 1" (lambda () (fib-internal 1)))
         (mk-benchmark-one "fib-internal 2" (lambda () (fib-internal 2)))
         (mk-benchmark-one "fib-internal 20" (lambda () (fib-internal 20)))
         (mk-benchmark-one "fib-internal 21" (lambda () (fib-internal 21))))
   (mk-benchmark-opts #:time-external #f)))

(define fib-external-group
  (mk-benchmark-group
   "externals"
   (list (mk-benchmark-one "fib 1" (lambda () (fib 1)))
         (mk-benchmark-one "fib 2" (lambda () (fib 2)))
         (mk-benchmark-one "fib 20" (lambda () (fib 20)))
         (mk-benchmark-one "fib 21" (lambda () (fib 21))))))

(run-benchmarks
 (mk-benchmark-group
  ""
  (list fib-internal-group fib-external-group)
  (mk-benchmark-opts #:gc-between #f #:itrs-per-trial 100)))

