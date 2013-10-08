#lang racket

(require "../src/benchmark.rkt")

(define files (list "fib5.rkt" "fib30.rkt"))

(define (jit-no-jit jit)
  (mk-benchmark-group
   (if jit "jit" "no jit")
   (map (lambda (f)
          (mk-shell-benchmark
           f
           (format (if jit "racket ~a" "racket -j ~a") f)))
    files)))

(define results
  (run-benchmarks
   (mk-benchmark-group "" (list (jit-no-jit #t) (jit-no-jit #f)))
   #:benchmark-opts (mk-benchmark-opts #:num-trials 50)))





