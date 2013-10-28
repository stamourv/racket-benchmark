#lang racket

(require benchmark plot)

(define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))
(define fib-group
  (list (bench-one "fib 18" (fib 18))
        (bench-one "fib 19" (fib 19))
        (bench-one "fib 20" (fib 20))
        (bench-one "fib 21" (fib 21))
        (bench-one "fib 22" (fib 22))))

(define results
  (run-benchmarks fib-group
                  (mk-bench-opts
                   #:num-trials 30
                   #:gc-between #f)))

(parameterize ([plot-x-ticks no-ticks])
  (plot-file
   #:title "fibonacci"
   #:x-label #f
   #:y-label "normalized time"
   (render-benchmark-alts
    (map (lambda (i) (format "fib ~a" i))
         (list 18 19 20 21 22))
    "fib 18"
    results)
   "fibonacci.pdf"))
