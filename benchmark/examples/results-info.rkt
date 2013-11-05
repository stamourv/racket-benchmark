#lang racket

(require benchmark rackunit)

(define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))
(define fib-group
  (mk-bench-group
   "fibs" (list (bench-one "fib 18" (fib 18))
                (bench-one "fib 19" (fib 19)))
   #:num-trials 30 #:gc-between #f))

(define results (run-benchmarks fib-group))

(define info+results (attach-linux-info results))

(record-results info+results "results-info")
(check-equal? (get-past-results "results-info") info+results)
