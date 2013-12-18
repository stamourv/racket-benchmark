#lang racket

(provide (struct-out benchmark-result))

;;;;;;;;;;;;;;;;;;;;;;;;; Benchmark Results ;;;;;;;;;;;;;;;;;;;;;;;;;

(struct benchmark-result
  (name            ;; string?
   opts            ;; (listof any/c)
   trial-times     ;; (listof num?)
   )
  #:prefab
  )
