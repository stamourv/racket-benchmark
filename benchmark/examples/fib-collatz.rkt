#lang racket

(require benchmark)

(define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))
(define fib-group
  (bgroup"fibs" (list (b1 (fib 1))
                      (b1 (fib 2))
                      (b1 (fib 20))
                      (b1 (fib 21)))))

(define (collatz n)
  (if (even? n)
      (/ n 2)
      (+ (* 3 n) 1)))

(define (collatz-range m)
  (for-each (lambda (n) (collatz n))
            (stream->list (in-range 0 m))))

(define collatz-group
  (bgroup "collatz" (list (b1 (collatz-range 1000))
                          (b1 (collatz-range 2000)))))

(define collatz-and-fib
  (bgroup ""
          (list fib-group collatz-group)
          (bopts #:gc-between #f)))

(run-benchmarks collatz-and-fib)
