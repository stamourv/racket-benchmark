#lang racket

(require (for-syntax syntax/parse))

(provide ;; single benchmark
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
         bresult)

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

;; mk-b1 : string? procedure? -> benchmark-one?
(define (mk-b1 name thunk
               [opts (bopts)] ;; benchmark-opts?
               )
  (benchmark-one thunk (struct-copy benchmark-opts opts [name name])))

;; m-command-or-proc : (or/c command procedure? nothing)

(struct shell-benchmark benchmark-one
  (configure        ;; m-command-or-proc
   build            ;; m-command-or-proc
   run              ;; m-command-or-proc
   extract-result   ;; bytes? -> benchmark-trial-time?
   clean            ;; m-command-or-proc
   )
  #:transparent
  )

(struct benchmark-group
  (benchmarks      ;; (listof benchmark-one?)
   opts            ;; benchmark-opts?
   )
  #:transparent
  )

;; mk-bgroup : string? (listof benchmark-one?) -> benchmark-group?
(define (mk-bgroup name benchmarks
                   [opts (bopts)] ;; benchmark-opts?
                   )
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
         #:name [name ""]                          ;; string?
         #:gc-between [gc-between nothing]         ;; boolean?
         #:num-trials [num-trials nothing]         ;; exact-integer?
         #:itrs-per-trial [itrs-per-trial nothing] ;; exact-integer?
         #:discard-first [discard-first nothing]   ;; boolean?
         #:time-external [time-external nothing])  ;; boolean?
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
  (configure-time  ;; exact-integer?
   build-time      ;; exact-integer?
   run-time        ;; exact-integer?
   clean-time      ;; exact-integer?
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
  (mean                ;; real?
   samples             ;; real?
   coeff-of-var        ;; real?
   conf-lb             ;; real?
   conf-ub             ;; real?
   conf-level          ;; real?
   )
  #:prefab
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Miscellaneous ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct nothing-s ())

;; nothing : nothing?
(define nothing (nothing-s))

;; nothing? : any/c -> boolean?
(define nothing? nothing-s?)
