#lang racket

(require benchmark)

(define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))
(define fib-group
  (bench-group"fibs" (list (bench-one (fib 1))
                      (bench-one (fib 2))
                      (bench-one (fib 20))
                      (bench-one (fib 21)))))

(define (collatz n)
  (if (even? n)
      (/ n 2)
      (+ (* 3 n) 1)))

(define (collatz-range m)
  (for-each (lambda (n) (collatz n))
            (stream->list (in-range 0 m))))

(define collatz-group
  (bench-group "collatz" (list (bench-one (collatz-range 1000))
                          (bench-one (collatz-range 2000)))))

(define collatz-and-fib
  (bench-group ""
          (list fib-group collatz-group)
          (mk-bench-opts #:gc-between #f)))

(run-benchmarks collatz-and-fib)
