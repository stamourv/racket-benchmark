#lang racket

;; To require on headless systems, which won't have plot.

(require "types.rkt"
         "macro.rkt"
         "results.rkt")

(provide run-benchmarks
         get-past-results
         record-results
         racket-time-extract-result)
