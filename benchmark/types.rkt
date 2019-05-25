#lang racket

(provide (struct-out benchmark-result) benchmark-logger)

;;;;;;;;;;;;;;;;;;;;;;;;; Benchmark Results ;;;;;;;;;;;;;;;;;;;;;;;;;

(struct benchmark-result
  (name            ;; any/c
   opts            ;; (listof any/c)
   trial-times     ;; (listof number?)
   )
  #:prefab
  )

(define benchmark-logger (make-logger 'benchmark (current-logger)))
