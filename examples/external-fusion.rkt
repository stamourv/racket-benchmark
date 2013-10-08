#lang racket

(require "../src/benchmark.rkt")

(require plot)

(define files (list "external/fusion.rkt"
                    "external/no-fusion.rkt"))

(define results
  (run-benchmarks
   (mk-benchmark-group
    ""
    (list
     (mk-benchmark-group
      "fusion"
      (list (mk-shell-benchmark "fusion.rkt" "racket external/fusion.rkt")))
     (mk-benchmark-group
      "no-fusion"
      (list (mk-shell-benchmark "no-fusion.rkt" "racket external/fusion.rkt"))))
    #:benchmark-opts (mk-benchmark-opts #:num-trials 31))))

(parameterize ([plot-x-ticks no-ticks])
  (plot-file
   (render-benchmark-alts (list "fusion" "no-fusion") results "no-fusion")
   "fusion-no-fusion.pdf"))
