#lang racket

(require benchmark plot "sanity-common.rkt")

(parameterize ([plot-x-ticks no-ticks]
                         [plot-x-tick-label-anchor 'top-right]
                         [plot-x-tick-label-angle 30]
                         [min-samples 1])
  (plot-file
   #:title "Linux vs Racket Sleep"
   #:x-label #f
   #:y-label "normalized time"
   (render-benchmark-alts
    (list "racket" "linux")
    "linux"
    (get-past-results results-file))
   "sanity.pdf"))
