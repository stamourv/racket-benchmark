#lang racket

(require benchmark plot)

;; (define sleep-times (list .33 .66 1.0))
(define sleep-times (list .25 .50 .75 1.0))
;;(define sleep-times (list .2 .4 .6 .8 1.0))

(define (sleep-external-bench n)
  (mk-shell-bench
   (format "sleep ~a" n)
   (format "( /usr/bin/time -p sleep ~a ) 2>&1" n)
   #:extract-result linux-time-extract-result))

(define benches (map sleep-external-bench sleep-times))

(define results
  (parameterize ([min-samples 1])
      (run-benchmarks benches
                  (mk-bench-opts #:num-trials 1 #:gc-between #f))))

(parameterize ([plot-x-ticks no-ticks]
                         [min-samples 1])
  (plot-file
   #:title "linux sleep"
   #:x-label #f
   #:y-label "normalized time"
   (render-benchmark-alts
    (map (lambda (t) (format "sleep ~a" t)) sleep-times)
    "sleep 0.25"
;;    "sleep 0.33"
    results)
   "linux-time.pdf"))
