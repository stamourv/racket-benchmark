#lang racket

(provide (struct-out benchmark-result) benchmark-logger benchmark-data)

;;;;;;;;;;;;;;;;;;;;;;;;; Benchmark Results ;;;;;;;;;;;;;;;;;;;;;;;;;

(struct benchmark-result
  (name            ;; string?
   opts            ;; (listof any/c)
   trial-times     ;; (listof num?)
   )
  #:prefab
  )

(define benchmark-logger (make-logger 'benchmark (current-logger)))

(define (benchmark-data benchmark)
  (map (λ (f) (f benchmark))
       (list benchmark-result-name benchmark-result-opts benchmark-result-trial-times)))
