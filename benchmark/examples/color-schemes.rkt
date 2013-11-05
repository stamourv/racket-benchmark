#lang racket

(require plot benchmark)

(define benches
  (mk-bench-group
   ""
   (list
    (mk-bench-group "A" (list (bench-one "1" (sleep 0.01))))
    (mk-bench-group "B" (list (bench-one "1" (sleep 0.01))))
    (mk-bench-group "C" (list (bench-one "1" (sleep 0.01))))
    (mk-bench-group "D" (list (bench-one "1" (sleep 0.01))))
    (mk-bench-group "E" (list (bench-one "1" (sleep 0.01))))
    (mk-bench-group "F" (list (bench-one "1" (sleep 0.01))))
    (mk-bench-group "G" (list (bench-one "1" (sleep 0.01))))
    (mk-bench-group "H" (list (bench-one "1" (sleep 0.01)))))
   #:gc-between #f
   #:itrs-per-trial 10
   #:num-trials 31))

(define results (run-benchmarks benches))

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
