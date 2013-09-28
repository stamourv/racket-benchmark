#lang racket

(require rackunit)
(require "../src/benchmark.rkt")

;; benchmark tests
(define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))
(define fib-group
  (mk-benchmark-group
   "fibs"
   (list (mk-benchmark-one "fib 1" (lambda () (fib 1)))
         (mk-benchmark-one "fib 2" (lambda () (fib 2)))
         (mk-benchmark-one "fib 20" (lambda () (fib 20)))
         (mk-benchmark-one "fib 21" (lambda () (fib 21))))))

(define (collatz n)
  (if (even? n)
      (/ n 2)
      (+ (* 3 n) 1)))

(define (collatz-range m)
  (for-each (lambda (n) (collatz n))
            (stream->list (in-range 0 m))))

(define collatz-group
  (mk-benchmark-group
   "collatz"
   (list
    (mk-benchmark-one "collatz 1000" (lambda () (collatz-range 1000)))
    (mk-benchmark-one "collatz 10000" (lambda () (collatz-range 10000))))
   (mk-benchmark-opts #:discard-first #f
                      #:num-trials 30
                      #:itrs-per-trial 30)))

(define collatz-and-fib
  (mk-benchmark-group "" (list fib-group collatz-group)
                      (mk-benchmark-opts #:gc-between #f)))

(run-benchmarks collatz-and-fib)

