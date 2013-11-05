#lang racket

(require benchmark plot)

(define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))
(define fib-bench
  (parameterize ([num-trials 10]
                 [gc-between #f])
    (bench-one (fib 21))))

(define results
  ;; reduce the required number of samples for calculating stats
  (parameterize ([min-samples 10]) (run-benchmarks fib-bench)))
