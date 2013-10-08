#lang racket

(require rackunit plot)
(require "../src/benchmark.rkt")

;; benchmark tests
(define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))

(define (fib-internal n)
;  (fib n)
  (time-internal (lambda () (for ([i 100]) (fib n))))
;  (fib n)
  )

(define (mk-fib-bench n fn)
  (mk-benchmark-one (format "fib ~a" n) (lambda () (fn n))))

(define fib-inputs (list 19 20 21))

(define fib-internal-group
  (mk-benchmark-group
   "internals"
   (map (lambda (n) (mk-fib-bench n fib-internal)) fib-inputs)
   (mk-benchmark-opts #:time-external #f)))

(define (fib-external-group n)
  (mk-benchmark-group
   (format "externals~a" n)
   (map (lambda (m) (mk-fib-bench m fib)) fib-inputs)))

(define num-external-groups 1)

(define external-nums (for/list ([i num-external-groups]) i))

(define external-names (map (lambda (n) (format "externals~a" n)) external-nums))

(define results
  (run-benchmarks
   (mk-benchmark-group ""
    (cons fib-internal-group (map fib-external-group external-nums))
    (mk-benchmark-opts #:gc-between #f #:num-trials 31 #:itrs-per-trial 100)))) 

(parameterize ([plot-x-ticks no-ticks])
  (plot-file
   (render-benchmark-alts (cons "internals" external-names) results)
   "internal-external.pdf"))
