#lang racket

(require rackunit)
(require "../src/benchmark.rkt")

;; benchmark tests
(define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))

(let* ([results
        (run-benchmarks
         (mk-benchmark-group
          "fib group"
          (list
           (mk-benchmark-one
            "fib 5" (lambda () (fib 5)))
           (mk-benchmark-one
            "fib 10" (lambda () (fib 10))))
          (mk-benchmark-opts #:gc-between #f))
         #:results-file-prefix "tmp-record-get")])
  (record-results results "record-get")
  (let ([read-results (get-past-results "record-get")])
    (check-equal? results read-results)))


