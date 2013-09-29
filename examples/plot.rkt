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

(define fib-vals (list 25 26))

(plot-benchmarks
 (map (lambda (x) (format "fibs/fib ~a" x)) fib-vals)
 (run-benchmarks
  (mk-benchmark-group
   "fibs"
   (map fib-bench fib-vals)))
 "fibs"
 "fibs.pdf")




