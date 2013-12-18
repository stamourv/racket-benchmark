#lang racket

(require "types.rkt"
         "macro.rkt"
         "plot.rkt"
         "results.rkt")

(provide ;; macro benchmarks
         run-benchmarks
         (all-from-out "plot.rkt")
         get-past-results
         record-results
         racket-time-extract-result)
