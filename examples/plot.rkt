#lang racket

(require "../src/benchmark.rkt")

(define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))

(define (fib-bench n)
  (mk-benchmark-one
   (format "fib ~a" n) 
    (lambda () (fib n))
    (mk-benchmark-opts
     #:gc-between #f
     #:plot-file "fib20.pdf")))

(plot-benchmark-result (car (run-benchmarks (fib-bench 20))))




