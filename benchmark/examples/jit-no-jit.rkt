#lang racket

(require plot benchmark)

(define files
  (list
   "external/fib5.rkt"
   "external/fib20.rkt"
   "external/collatz1000.rkt"))

(define (jit-no-jit jit)
  (parameterize ([num-trials 30])
    (mk-bench-group
     (if jit "jit" "no jit")
     (map (lambda (f)
            (mk-racket-file-bench
             (cadr (regexp-match #rx"([^.]+)\\.rkt" f))
             f
             (if jit (list) (list "-j"))))
          files))))

(define results
  (run-benchmarks (list (jit-no-jit #t) (jit-no-jit #f))))

(record-results results "jit-no-jit.bench")

(parameterize ([plot-x-ticks no-ticks])
  (plot-file
   #:title "jit vs no jit"
   #:x-label #f
   #:y-label "normalized time"
   (render-benchmark-alts (list "jit" "no jit") "jit"
                          (get-past-results "jit-no-jit.bench"))
   "jit-no-jit.pdf"))
