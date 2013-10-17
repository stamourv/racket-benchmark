#lang racket

(require benchmark rackunit)

(define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))
(define fib-group
  (bench-group
   "fibs" (list (bench-one "fib 18" (fib 18))
                (bench-one "fib 19" (fib 19)))))

(define results
  (run-benchmarks fib-group
                  (mk-bench-opts
                   #:num-trials 30
                   #:gc-between #f)))

(define info+results (attach-linux-info results))

(record-results info+results "results-info")
(check-equal? (get-past-results "results-info") info+results)
