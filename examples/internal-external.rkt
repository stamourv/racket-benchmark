#lang racket

(require plot)
(require "../src/benchmark.rkt")

(define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))

(define (fib-internal n)
  (define (do-fib) (for ([i 500]) (fib n)))
  (do-fib)
  (time-internal do-fib))

(define (mk-fib-bench n fn)
  (b1 (format "fib ~a" n) (fn n)))

(define fib-inputs (list 19 20 21))

(define fib-internal-group
  (mk-bgroup
   "internals"
   (map (lambda (n) (mk-fib-bench n fib-internal)) fib-inputs)
   (bopts #:time-external #f)))

(define fib-external-group
  (mk-bgroup
   "externals"
   (map (lambda (m) (mk-fib-bench m fib)) fib-inputs)))

(define results
  (run-benchmarks
   (mk-bgroup ""
    (list fib-internal-group fib-external-group)
    (bopts #:gc-between #f)))) 

(parameterize ([plot-x-ticks no-ticks])
  (plot-file
   (render-benchmark-alts (list "internals" "externals") results "externals")
   "internal-external.pdf"))
