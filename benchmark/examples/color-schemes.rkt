#lang racket

(require plot benchmark)

(define benches
  (bgroup   
   ""
   (list
    (bgroup "A" (list (b1 "1" (sleep 0.01))))
    (bgroup "B" (list (b1 "1" (sleep 0.01))))
    (bgroup "C" (list (b1 "1" (sleep 0.01))))
    (bgroup "D" (list (b1 "1" (sleep 0.01))))
    (bgroup "E" (list (b1 "1" (sleep 0.01))))
    (bgroup "F" (list (b1 "1" (sleep 0.01))))
    (bgroup "G" (list (b1 "1" (sleep 0.01))))
    (bgroup "H" (list (b1 "1" (sleep 0.01))))
    )))

(define results
  (run-benchmarks
   benches
   (bopts
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
    results
    "A")
   "color-schemes.pdf"))
