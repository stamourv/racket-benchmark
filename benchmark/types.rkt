#lang racket

(provide (struct-out benchmark-result) benchmark-logger)

;;;;;;;;;;;;;;;;;;;;;;;;; Benchmark Results ;;;;;;;;;;;;;;;;;;;;;;;;;

(struct benchmark-result
  (name            ;; string?
   opts            ;; (listof any/c)
   trial-times     ;; (listof num?)
   )
  #:prefab
  )

(define benchmark-logger (make-logger 'benchmark (current-logger)))
