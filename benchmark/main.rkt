#lang racket

(require "types.rkt"
         "stats.rkt"
         "print.rkt"
         "time.rkt"
         "macro.rkt"
         "plot.rkt"
         "results.rkt")

(provide ;; macro benchmarks
         run-benchmarks

         ;; macro and micro benchmraks
         (struct-out benchmark-trial-time)
         (struct-out shell-benchmark-trial-time)
         (all-from-out "plot.rkt")
         get-past-results
         record-results
         racket-time-extract-result)
