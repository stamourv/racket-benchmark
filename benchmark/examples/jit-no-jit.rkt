#lang racket

(require plot benchmark)

(define files
  (list
   "external/fib5.rkt"
   "external/fib20.rkt"
   "external/collatz1000.rkt"))

(define (jit-no-jit jit)
  (bench-group
   (if jit "jit" "no jit")
   (map (lambda (f)
          (mk-shell-benchmark
           (cadr (regexp-match #rx"([^.]+)\\.rkt" f))
           (format (if jit "racket ~a" "racket -j ~a") f)))
    files)))

(define results
  (run-benchmarks
   (mk-bench-group "" (list (jit-no-jit #t) (jit-no-jit #f)))
   (mk-bench-opts #:num-trials 31)))

(record-results results "jit-no-jit.bench")

(parameterize ([plot-x-ticks no-ticks])
  (plot-file
   #:title "jit vs no jit"
   #:x-label #f
   #:y-label "normalized time"
   (render-benchmark-alts (list "jit" "no jit") "jit"
                          (get-past-results "jit-no-jit.bench"))
   "jit-no-jit.pdf"))
