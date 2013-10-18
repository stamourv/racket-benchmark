#lang racket

(require benchmark plot)

(define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))
(define fib-group (bench-group "fibs" (list (bench-one (fib 21)))))

(define results
  ;; reduce the required number of samples for calculating stats
  (parameterize ([min-samples 5])
    (run-benchmarks fib-group
                    (mk-bench-opts
                     #:num-trials 10
                     #:gc-between #f))))
