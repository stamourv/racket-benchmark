#lang racket

(require math/statistics)
(require "types.rkt")

(provide
 mk-benchmark-one
 mk-benchmark-group
 mk-benchmark-opts
 run-benchmarks
 time-one
 (struct-out benchmark-trial-time)
 )

;; for internal timing
(define timing-logger (make-logger 'timing-logger))
(define timing-logger-receiver (make-log-receiver timing-logger 'info))
(define (report-time time)
  (log-message timing-logger 'info "" time))

;; this module implements a simple benchmarking library
(define (append-opts o1 o2)
  (define (filter-nothing lst)
    (filter (lambda (x) (not (nothing-s? x))) lst))
  (define (opt-val fn)
    (let ([filtered-opts (filter-nothing (map fn (filter-nothing (list o1 o2))))])
      (if (null? filtered-opts)
          nothing
          (car filtered-opts))))
  (let ([gc (opt-val benchmark-opts-gc-between-each)]
        [trials (opt-val benchmark-opts-num-trials)]
        [itrs (opt-val benchmark-opts-itrs-per-trial)]
        [discard (opt-val benchmark-opts-discard-first)]
        [time-external (opt-val benchmark-opts-time-external)])
    (benchmark-opts gc trials itrs discard time-external)))

(define (append-default-opts o) (append-opts o default-opts))

;; string? string? -> string?
(define (relname a b)
  (if (equal? "" a) b (string-append a "/" b)))

;; (benchmark-one? or benchmark-group?) -> void
(define (run-benchmarks benchmarks [benchmark-opts nothing])
  (define (run-benchmarks-aux bs name opts)
    ;; run a benchmark (benchmark-one? or benchmark-group?)
    ;; from benchmark-group? bs, remembering the name and
    ;; benchmark-options? of the bs
    (define (run-group-elem e)
      (run-benchmarks-aux
       e
       (relname name (benchmark-group-name bs))
       (append-opts (benchmark-group-opts bs) opts)))
    ;; run a benchmark-one?
    (define (run-one b)
      (let ([final-opts
             (append-default-opts (append-opts (benchmark-one-opts b) opts))])
        (displayln
         (format "Running benchmark: ~a, ~a trials, ~a runs per trial"
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
                   (if (benchmark-opts-time-external final-opts)
                       (let-values
                           ([(_ cpu real gc)
                             (time-apply
                              (lambda ()
                                (for ([j (benchmark-opts-itrs-per-trial final-opts)])
                                  ((benchmark-one-thunk b))))
                              '())])
                         (benchmark-trial-time cpu real gc))
                       (begin
                         ((benchmark-one-thunk b))
                         (vector-ref (sync timing-logger-receiver) 2)
                         ))))])
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
  (run-benchmarks-aux benchmarks "" benchmark-opts))

;; list? (benchmark-trial-time?) -> benchmark-trial-times?
(define (raw-to-stats times)
  (let ([cpu (map benchmark-trial-time-cpu times)]
        [real (map benchmark-trial-time-real times)]
        [gc (map benchmark-trial-time-gc times)])
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

;; procedure? -> void
(define (time-one thunk)
  (let-values ([(_ cpu real gc) (time-apply thunk '())])
    (report-time (benchmark-trial-time cpu real gc))))
