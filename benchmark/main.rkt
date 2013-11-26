#lang racket

(require "types.rkt"
         "stats.rkt"
         "print.rkt"
         "time.rkt"
         "macro.rkt"
         "plot.rkt"
         "results.rkt"
         "run.rkt")

(provide ;; macro benchmarks
         run-macro-benchmarks

         ;; micro benchmarks
         make-bench-one
         bench-one
         gc-between
         num-trials
         itrs-per-trial
         discard-first
         manual-report-time
         run-benchmarks
         time-internal

         ;; macro and micro benchmraks
         (struct-out benchmark-trial-time)
         (struct-out shell-benchmark-trial-time)
         (all-from-out "plot.rkt")
         get-past-results
         record-results
         racket-time-extract-result
         linux-time-extract-result)
