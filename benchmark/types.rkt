#lang racket

(require (for-syntax syntax/parse))

(provide ;; single benchmark
         (struct-out benchmark-one)
         mk-bench-one
         bench-one
         ;; shell benchmarks
         (struct-out shell-benchmark)
         ;; grouping benchmarks together by name
         (struct-out benchmark-group)
         mk-bench-group
         ;; options
         (struct-out benchmark-opts)
         mk-bench-opts
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
         ;; for persistent results
         (struct-out bench-results)
         mk-bench-result)

;;;;;;;;;;;;;;;;;;;;;;;;; Specifying Benchmarks ;;;;;;;;;;;;;;;;;;;;;;;;;

(struct benchmark-one
  (name            ;; string?
   thunk           ;; procedure?
   opts            ;; benchmark-opts?
   )
  #:transparent
  )

(define-syntax (bench-one stx)
  (syntax-parse stx
    [(_ n:expr b:expr opts:expr)
     #'(mk-bench-one n (thunk b) opts)]
    [(_ n:expr b:expr)
     #'(mk-bench-one n (thunk b))]
    [(_ b:expr)
     #'(mk-bench-one (format "~a" 'b) (thunk b))]))

;; mk-bench-one : string? procedure? -> benchmark-one?
(define (mk-bench-one name thunk
               [opts (mk-bench-opts)] ;; benchmark-opts?
               )
  (benchmark-one name thunk opts))

;; m-command-or-proc : (or/c command procedure? nothing)

(struct shell-benchmark
  (configure        ;; m-command-or-proc
   build            ;; m-command-or-proc
   run              ;; m-command-or-proc
   extract-result   ;; bytes? -> benchmark-trial-time?
   clean            ;; m-command-or-proc
   )
  #:transparent
  )

(struct benchmark-group
  (name            ;; string?
   benchmarks      ;; (listof benchmark-one?)
   opts            ;; benchmark-opts?
   )
  #:transparent
  )

;; mk-bench-group : string? (listof benchmark-one?) -> benchmark-group?
(define (mk-bench-group name benchmarks
                   [opts (mk-bench-opts)] ;; benchmark-opts?
                   )
  (benchmark-group name benchmarks opts))

(struct benchmark-opts
  (gc-between-each ;; boolean?
   num-trials      ;; exact-integer?
   itrs-per-trial  ;; exact-integer?
   discard-first   ;; boolean?
   manual-report-time   ;; boolean?
   )
  #:prefab
  )

(define (mk-bench-opts
         #:gc-between [gc-between nothing]                  ;; boolean?
         #:num-trials [num-trials nothing]                  ;; exact-integer?
         #:itrs-per-trial [itrs-per-trial nothing]          ;; exact-integer?
         #:discard-first [discard-first nothing]            ;; boolean?
         #:manual-report-time [manual-report-time nothing]) ;; boolean?
  (benchmark-opts gc-between num-trials itrs-per-trial
                  discard-first manual-report-time))

(define default-opts (benchmark-opts #t 100 500 #t #f))

;;;;;;;;;;;;;;;;;;;;;;;;; Benchmark Results ;;;;;;;;;;;;;;;;;;;;;;;;;

(struct benchmark-result
  (name            ;; string?
   opts            ;; benchmark-opts?
   trial-stats     ;; benchmark-trial-stats?
   )
  #:prefab
  )

(define mk-bench-result benchmark-result)

(struct bench-results
  (results    ;; (listof benchmark-result?)
   time/date  ;; date*
   )
  #:prefab
  )

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
