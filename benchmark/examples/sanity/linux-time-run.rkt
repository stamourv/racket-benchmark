#lang racket

(require benchmark plot "linux-time.rkt")

(define sleep-times (list .25 .50 .75 1.0))

(define benches
  (parameterize ([num-trials 1]
                 [gc-between #f])
    (map sleep-external-bench sleep-times)))

(define results
  (parameterize ([min-samples 1])
    (run-benchmarks benches)))

(parameterize ([plot-x-ticks no-ticks]
                         [min-samples 1])
  (plot-file
   #:title "linux sleep"
   #:x-label #f
   #:y-label "normalized time"
   (render-benchmark-alts
    (map (lambda (t) (format "sleep ~a" t)) sleep-times)
    "sleep 0.25"
    results)
   "linux-time.pdf"))
