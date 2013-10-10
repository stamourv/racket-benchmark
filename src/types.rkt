#lang racket

(require (for-syntax syntax/parse))

(provide
 ;; single benchmark
 (struct-out benchmark-one)
 mk-b1
 b1
 ;; shell benchmarks
 (struct-out shell-benchmark)
 ;; grouping benchmarks together by name
 (struct-out benchmark-group)
 mk-bgroup
 bgroup
 ;; options
 (struct-out benchmark-opts)
 bopts
 default-opts
 ;; for representing unset fields in options
 nothing
 nothing?
 ;; time of a single trial
 (struct-out benchmark-trial-time)
 (struct-out shell-benchmark-trial-time)
 ;; time of multiple trials
 (struct-out benchmark-trial-stats)
 (struct-out measured-value)
 (struct-out benchmark-result)
 bresult
 )

;;;;;;;;;;;;;;;;;;;;;;;;; Specifying Benchmarks ;;;;;;;;;;;;;;;;;;;;;;;;;

(struct benchmark-one
  (thunk           ;; procedure?
   opts            ;; benchmark-opts?
   )
  #:transparent
  )

(define-syntax (b1 stx)
  (syntax-parse stx
    [(_ n:expr b:expr opts:expr)
     #'(benchmark-one (lambda () b)
                      (struct-copy
                       benchmark-opts
                       opts
                       [name n]))]
    [(_ n:expr b:expr)
     #'(benchmark-one (lambda () b) (bopts #:name n))]
    [(_ b:expr)
     #'(benchmark-one
        (lambda () b)
        (bopts #:name (format "~a" 'b)))]))

(define (mk-b1 name thunk [opts (bopts)])
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

(define (mk-bgroup name benchmarks [opts (bopts)])
  (benchmark-group benchmarks (struct-copy benchmark-opts opts [name name])))

(define-syntax (bgroup stx)
  (syntax-parse stx
    [(_ n:expr bs:expr opts:expr)
     #'(benchmark-group bs
                        (struct-copy
                         benchmark-opts
                         opts
                         [name n]))]
    [(_ n:expr bs:expr)
     #'(benchmark-group bs (bopts #:name n))]))

(struct benchmark-opts
  (name            ;; string?
   gc-between-each ;; boolean?
   num-trials      ;; exact-integer?
   itrs-per-trial  ;; exact-integer?
   discard-first   ;; boolean?
   time-external   ;; boolean?
   )
  #:prefab
  )

(define (bopts
         #:name [name ""]
         #:gc-between [gc-between nothing]
         #:num-trials [num-trials nothing]
         #:itrs-per-trial [itrs-per-trial nothing]
         #:discard-first [discard-first nothing]
         #:time-external [time-external nothing])
  (benchmark-opts name gc-between num-trials itrs-per-trial
                  discard-first time-external))

(define default-opts (benchmark-opts "" #t 100 500 #t #t))

;;;;;;;;;;;;;;;;;;;;;;;;; Benchmark Results ;;;;;;;;;;;;;;;;;;;;;;;;;

(struct benchmark-result
  (opts            ;; benchmark-opts?
   trial-stats     ;; benchmark-trial-stats?
   )
  #:prefab
  )

(define bresult benchmark-result)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Times ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(struct benchmark-trial-stats
  (cpu             ;; measured-value?
   real            ;; measured-value?
   gc              ;; measured-value?
   )
  #:prefab
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Statistics ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Miscellaneous ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct nothing-s ())
(define nothing (nothing-s))
(define nothing? nothing-s?)
