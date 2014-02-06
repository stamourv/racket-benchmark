#lang racket

(require benchmark plot)

(void (run-benchmarks
 #:num-trials 3
 '(1 2)
 '(("a" "b"))
 (lambda (how-long version)
   (time (sleep how-long)))
 #:results-file "incremental-results"))

(define results (get-past-results "incremental-results"))

(plot-file (render-benchmark-alts '("a") results)
           "incremental-results.pdf"
           #:x-label #f #:y-label #f)
