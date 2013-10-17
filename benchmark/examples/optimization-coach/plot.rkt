:#lang racket

(require plot benchmark "common.rkt")

(define results
  (get-past-results results-file))

(parameterize ([plot-x-ticks no-ticks]
               [plot-x-tick-label-anchor 'top-right]
               [plot-x-tick-label-angle 30])
  (plot-file
   #:title "optimization coach"
   #:x-label #f
   #:y-label "normalized time"
   (render-benchmark-alts (map car optimization-type-files) "orig" results)
   "optimization-coach.pdf"))
