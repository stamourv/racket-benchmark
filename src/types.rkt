#lang racket

(provide
 nothing
 nothing?
 ;; single benchmark
 (struct-out benchmark-one)
 mk-benchmark-one
 ;; shell benchmarks
 (struct-out shell-benchmark)
 ;; grouping benchmarks together by name
 (struct-out benchmark-group)
 mk-benchmark-group
 ;; options
 (struct-out benchmark-opts)
 mk-benchmark-opts
 default-opts
 ;; time of a single trial
 (struct-out benchmark-trial-time)
 (struct-out shell-benchmark-trial-time)
 ;; time of multiple trials
 (struct-out benchmark-trial-stats)
 (struct-out measured-value)
 ;; result
 (struct-out benchmark-result)
 mk-benchmark-result
 ;; comparison of benchmarks
 (struct-out benchmark-comparison)
 mk-benchmark-comparison
 )

(struct nothing-s ())
(define nothing (nothing-s))
(define nothing? nothing-s?)

(struct benchmark-one
  (thunk           ;; procedure?
   opts            ;; benchmark-opts?
   )
  #:transparent
  )

(define (mk-benchmark-one name thunk [opts (mk-benchmark-opts)])
  (benchmark-one thunk (struct-copy benchmark-opts opts [name name])))

(struct shell-benchmark benchmark-one
  (configure
   build
   run
   extract-result
   clean
   )
  #:transparent
  )

(struct benchmark-group
  (benchmarks      ;; list? of benchmark-one?
   opts            ;; benchmark-opts?
   )
  #:transparent
  )

(define (mk-benchmark-group name benchmarks [opts (mk-benchmark-opts)])
  (benchmark-group benchmarks (struct-copy benchmark-opts opts [name name])))

(struct benchmark-opts
  (name            ;; string?
   gc-between-each ;; boolean?
   num-trials      ;; exact-integer?
   itrs-per-trial  ;; exact-integer?
   discard-first   ;; boolean?
   time-external   ;; boolean?
   plot-file       ;; string?
   )
  #:prefab
  )

(define (mk-benchmark-opts
         #:name [name ""]
         #:gc-between [gc-between nothing]
         #:num-trials [num-trials nothing]
         #:itrs-per-trial [itrs-per-trial nothing]
         #:discard-first [discard-first nothing]
         #:time-external [time-external nothing]
         #:plot-file [plot-file nothing])
  (benchmark-opts name gc-between num-trials itrs-per-trial
                  discard-first time-external plot-file))

(define default-opts (benchmark-opts "" #t 100 500 #t #t #f))

(struct benchmark-result
  (opts            ;; benchmark-opts?
   trial-stats     ;; benchmark-trial-stats?
   )
  #:prefab
  )

(define mk-benchmark-result benchmark-result)

(struct benchmark-comparison
  (result          ;; 'sig-improvement 'sig-regression 'not-sig
   opts            ;; benchmark-opts?
   )
  #:transparent
  )

(define mk-benchmark-comparison benchmark-comparison)

;; raw time of a single trial
(struct benchmark-trial-time
  (cpu             ;; exact-integer?
   real            ;; exact-integer?
   gc              ;; exact-integer?
   )
  #:prefab
  )

(struct shell-benchmark-trial-time benchmark-trial-time
  (configure-time  ;; flonum?
   build-time      ;; flonum?
   run-time        ;; flonum?
   clean-time      ;; flonum?
   )
  #:prefab
  )

;; trial times
(struct benchmark-trial-stats
  (cpu             ;; measured-value?
   real            ;; measured-value?
   gc              ;; measured-value?
   )
  #:prefab
  )

(struct measured-value
  (mean                ;; flonum?
   samples             ;; list? (flonum?)
   coeff-of-var        ;; flonum?
   conf-lb             ;; flonum?
   conf-ub             ;; flonum?
   conf-level          ;; flonum?
   )
  #:prefab
  )

