#lang racket

(require plot benchmark)

(define benches
  (bench-group   
   ""
   (list
    (bench-group "A" (list (bench-one "1" (sleep 0.01))))
    (bench-group "B" (list (bench-one "1" (sleep 0.01))))
    (bench-group "C" (list (bench-one "1" (sleep 0.01))))
    (bench-group "D" (list (bench-one "1" (sleep 0.01))))
    (bench-group "E" (list (bench-one "1" (sleep 0.01))))
    (bench-group "F" (list (bench-one "1" (sleep 0.01))))
    (bench-group "G" (list (bench-one "1" (sleep 0.01))))
    (bench-group "H" (list (bench-one "1" (sleep 0.01))))
    )))

(define results
  (run-benchmarks
   benches
   (mk-bench-opts
    #:gc-between #f
    #:itrs-per-trial 10
    #:num-trials 31)))

(parameterize ([plot-x-ticks no-ticks]
               [current-benchmark-color-scheme black-white-color-scheme-long])
  (plot-file
   #:title "sets"
   #:y-label "normalized time"
   #:x-label #f
   (render-benchmark-alts
    (list "A" "B" "C" "D" "E" "F" "G" "H")
    "A"
    results)
   "color-schemes.pdf"))
