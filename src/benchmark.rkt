#lang racket

(require math/statistics)
(require "types.rkt")

(provide
 mk-benchmark-one
 mk-benchmark-group
 mk-benchmark-opts
 run-benchmarks)

;; this module implements a simple benchmarking library
(define (append-opts opts)
  (define (filter-nothing lst)
    (filter (lambda (x) (not (nothing-s? x))) lst))
  (define (opt-val fn)
    (let ([filtered-opts (filter-nothing (map fn (filter-nothing opts)))])
      (if (null? filtered-opts)
          (fn default-opts)
          (car filtered-opts))))
  (let ([gc (opt-val benchmark-opts-gc-between-each)]
        [trials (opt-val benchmark-opts-num-trials)]
        [itrs (opt-val benchmark-opts-itrs-per-trial)]
        [discard (opt-val benchmark-opts-discard-first)]
        [time-external (opt-val benchmark-opts-time-external)])
    (benchmark-opts gc trials itrs discard time-external)))

;; running benchmarks
(define (run-benchmarks benchmarks [benchmark-opts nothing])
  (define (relname a b)
    (if (equal? "" a) b (string-append a "/" b)))
  (define (run-benchmark bs name opts)
    (define (run-group-elem b)
      (run-benchmark
       b
       (relname name (benchmark-group-name bs))
       (append-opts (list (benchmark-group-opts bs) opts))))
    (define (run-one b)
      (let ([final-opts (append-opts (list (benchmark-one-opts b) opts))])
        (displayln (format "Running benchmark: ~a, ~a trials, ~a runs per trial"
                           (relname name (benchmark-one-name b))
                           (benchmark-opts-num-trials final-opts)
                           (benchmark-opts-itrs-per-trial final-opts)))
        (let ([times
               ;; for each trial
               (for/list ([i (benchmark-opts-num-trials final-opts)])
                 (begin
                   (when (benchmark-opts-gc-between-each final-opts)
                     (collect-garbage)
                     (collect-garbage)
                     (collect-garbage))
                   (let-values
                       ([(_ cpu real gc)
                         (time-apply
                          (lambda ()
                            (for ([j (benchmark-opts-itrs-per-trial final-opts)])
                              (apply (benchmark-one-thunk bs) '())))
                          '())])
                   (benchmark-time cpu real gc))))])
          (print-times
           (raw-to-stats
            (if (benchmark-opts-discard-first final-opts)
                (cdr times)
                times))))))
    (cond
     [(benchmark-group? bs)
      (map run-group-elem (benchmark-group-benchmarks bs))]
     [(benchmark-one? bs) (run-one bs)]
     [else
      (error "Invalid benchmark: expected benchmark? or benchmark-group?")]))
  (run-benchmark benchmarks "" benchmark-opts))

(define (raw-to-stats times)
  (let ([cpu (map benchmark-time-cpu times)]
        [real (map benchmark-time-real times)]
        [gc (map benchmark-time-gc times)])
    (benchmark-trial-times (calculate-stats cpu)
                           (calculate-stats real)
                           (calculate-stats gc))))

;; list? (num?) -> measured-value?
(define (calculate-stats vals)
  (measured-value
   (mean vals)
   (if (= 0 (mean vals))
       ;; TODO: what is the appropriate return value?
       +inf.0
       (coeff-of-variation vals))))

;; list? (num?) -> num?
(define (coeff-of-variation vals)
  (/ (stddev vals) (mean vals)))

;; list? (benchmark-trial-times?) -> void
(define (print-times trial-times)
  (printf
   "cpu: ~a\nreal: ~a\ngc: ~a\n"
   (show-measured-value (benchmark-trial-times-cpu trial-times))
   (show-measured-value (benchmark-trial-times-real trial-times))
   (show-measured-value (benchmark-trial-times-gc trial-times))))

;; measured-value? -> string?
(define (show-measured-value mv)
  (format "mean: ~a, coeff-of-var: ~a"
          (exact->inexact (measured-value-mean mv))
          (exact->inexact (measured-value-coeff-of-var mv))))

;; tests

;; append opts tests
;; (define t1 (benchmark-opts #t 1))
;; (define f2 (benchmark-opts #f 2))
;; (define n2 (benchmark-opts nothing 2))
;; (define fn (benchmark-opts #f nothing))
;; (define nn (benchmark-opts nothing nothing))

;; (check-equal? (append-opts (list t1 f2)) t1 "'(t1 f2) -> t1")
;; (check-equal? (append-opts (list f2 t1)) f2 "'(f2 t1) -> f2")
;; (check-equal? (append-opts (list fn f2)) f2 "'(fn f2) -> f2")
;; (check-equal? (append-opts (list fn n2)) f2 "'(fn n2) -> f2")
;; (check-equal? (append-opts (list nn f2)) f2 "'(nn f2) -> f2")
;; (check-equal? (append-opts (list nn)) default-opts "'(nn) --> default")
;; (check-equal? (append-opts '()) default-opts "'() -> default")
;; (check-equal? (append-opts (list nn nn)) default-opts "'(nn nn) -> default")

