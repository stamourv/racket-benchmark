#lang racket

(require "types.rkt")

(provide
 print-times
 show-measured-value
 )

;; list? (benchmark-trial-stats?) -> void
(define (print-times trial-times)
  (printf
   "cpu: ~a\nreal: ~a\ngc: ~a\n"
   (show-measured-value (benchmark-trial-stats-cpu trial-times))
   (show-measured-value (benchmark-trial-stats-real trial-times))
   (show-measured-value (benchmark-trial-stats-gc trial-times))))

;; measured-value? -> string?
(define (show-measured-value mv)
  (format "mean: ~a, coeff-of-var: ~a"
          (exact->inexact (measured-value-mean mv))
          (exact->inexact (measured-value-coeff-of-var mv))))
