#lang racket

(require rackunit)
(require "../src/benchmark.rkt")

;; benchmark tests
(define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))
(run-benchmarks
 (mk-benchmark-one "fib" (lambda () (fib 15)))
 #:results-file-prefix "fib")

