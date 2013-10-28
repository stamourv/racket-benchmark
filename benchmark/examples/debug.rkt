#lang racket

(require benchmark plot)

(define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))
(define fib-bench (bench-one (fib 21)))

(define results
  ;; reduce the required number of samples for calculating stats
  (parameterize ([min-samples 5])
    (run-benchmarks fib-bench
                    (mk-bench-opts
                     #:num-trials 10
                     #:gc-between #f))))
