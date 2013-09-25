#lang racket

(provide
 (struct-out nothing-s)
 nothing
 ;; single benchmark
 (struct-out benchmark-one)
 mk-benchmark-one
 ;; grouping benchmarks together by name
 (struct-out benchmark-group)
 mk-benchmark-group
 ;; options
 (struct-out benchmark-opts)
 mk-benchmark-opts
 default-opts
 ;; time of a single trial
 (struct-out benchmark-trial-time)
 ;; time of multiple trials
 (struct-out benchmark-trial-times)
 (struct-out measured-value)
 )

;; define data representation

(struct nothing-s ())
(define nothing (nothing-s))

(struct benchmark-one
  (name            ;; string?
   thunk           ;; procedure?
   opts            ;; benchmark-opts?
   )
  #:transparent
  )

(define (mk-benchmark-one name thunk [opts nothing])
  (benchmark-one name thunk opts))

(struct benchmark-group
  (name            ;; string?
   benchmarks      ;; list? of benchmark-one?
   opts            ;; benchmark-opts?
   )
  #:transparent
  )

(define (mk-benchmark-group name benchmarks [opts nothing])
  (benchmark-group name benchmarks opts))

(struct benchmark-opts
  (gc-between-each ;; boolean?
   num-trials      ;; exact-integer?
   itrs-per-trial  ;; exact-integer?
   discard-first   ;; boolean?
   time-external   ;; boolean?
   )
  #:transparent
  )

(define (mk-benchmark-opts
         #:gc-between [gc-between nothing]
         #:num-trials [num-trials nothing]
         #:itrs-per-trial [itrs-per-trial nothing]
         #:discard-first [discard-first nothing]
         #:time-external [time-external nothing])
  (benchmark-opts gc-between num-trials itrs-per-trial discard-first time-external))

(define default-opts (benchmark-opts #t 50 1000 #t #t))

;; raw time of a single trial
(struct benchmark-trial-time
  (cpu             ;; exact-integer?
   real            ;; exact-integer?
   gc              ;; exact-integer?
   )
  #:transparent
  )

;; trial times
(struct benchmark-trial-times
  (cpu            ;; measured-value?
   real            ;; measured-value?
   gc              ;; measured-value?
   )
  #:transparent
  )

(struct measured-value
  (mean                ;; flonum?
   coeff-of-var        ;; flonum?
   ;; conf-lb             ;; flonum?
   ;; conf-ub             ;; flonum?
   ;; conf-level          ;; flonum?
   )
  #:transparent
  )
