#lang racket

(require "types.rkt"
         "stats.rkt"
         "print.rkt"
         "time.rkt"
         "external.rkt"
         "plot.rkt"
         "results.rkt")
(require math/statistics)

(provide mk-b1
         b1
         mk-bgroup
         bgroup
         mk-shell-benchmark
         bopts
         run-benchmarks
         time-internal
         benchmark-trial-time
         (all-from-out "plot.rkt")
         get-past-results
         record-results)

;; append-opts : benchmark-opts? benchmark-opts? -> benchmark-opts?
(define (append-opts o1 o2)
  (define (filter-nothing lst)
    (filter (lambda (x) (not (nothing? x))) lst))
  (define (relname a b)
    (if (equal? "" a) b (string-append a "/" b)))
  (define (name-val o)
    (if (or (nothing? o) (nothing? (benchmark-opts-name o)))
        ""
        (benchmark-opts-name o)))
  (define (opt-val fn)
    (let ([filtered-opts
           (filter-nothing (map fn (filter-nothing (list o1 o2))))])
      (if (null? filtered-opts)
          nothing
          (car filtered-opts))))
  (let ([name (relname (name-val o2) (name-val o1))]
        [gc (opt-val benchmark-opts-gc-between-each)]
        [trials (opt-val benchmark-opts-num-trials)]
        [itrs (opt-val benchmark-opts-itrs-per-trial)]
        [discard (opt-val benchmark-opts-discard-first)]
        [time-external (opt-val benchmark-opts-time-external)])
    (benchmark-opts name gc trials itrs discard time-external)))

;; append-default-opts : benchmark-opts? -> benchmark-opts?
(define (append-default-opts o) (append-opts o default-opts))

;; run-benchmarks : (or/c benchmark-one? benchmark-group?)
;;                  -> (listof benchmark-result?)
(define (run-benchmarks bs [opts nothing])
  ;; run a benchmark (benchmark-one? or benchmark-group?)
  ;; from benchmark-group? bs after combining options of
  ;; group with those optional opts
  (define (run-group-elem e)
    (run-benchmarks
     e
     (append-opts (benchmark-group-opts bs) opts)))
  ;; run a benchmark-one?
  (define (run-one b)
    (let*
        ;; compute final options, filling in with default values as needed
        ([final-opts
          (append-default-opts
           (append-opts
            (benchmark-one-opts b) opts))]
         [itrs-per-trial (benchmark-opts-itrs-per-trial final-opts)])
      (displayln
       (format "Running benchmark: ~a, ~a trials, ~a runs per trial"
               (benchmark-opts-name final-opts)
               (benchmark-opts-num-trials final-opts)
               (benchmark-opts-itrs-per-trial final-opts)))
      (let*
          ([times
            ;; for each trial
            (for/list ([i (benchmark-opts-num-trials final-opts)])
              (begin
                (when (benchmark-opts-gc-between-each final-opts)
                  (collect-garbage)
                  (collect-garbage)
                  (collect-garbage))
                (cond
                 ;; time external (using time-apply)
                 [(benchmark-opts-time-external final-opts)
                  (let-values
                      ([(_ cpu real gc)
                        (time-apply
                         (thunk
                          ;; for each iteration within a trial
                          (for ([j (in-range 0 itrs-per-trial)])
                            ((benchmark-one-thunk b))))
                         '())])
                    (benchmark-trial-time cpu real gc))]
                 ;; benchmark will do its own timing using report-time
                 [else ((benchmark-one-thunk b)) (receive-time)])))]
           [trimmed-times
            (if (benchmark-opts-discard-first final-opts)
                (cdr times)
                times)]
           [stats (raw-to-stats trimmed-times)])
        (print-times (raw-to-stats trimmed-times))
        (bresult final-opts stats))))
  (cond
   [(benchmark-group? bs)
    (append-map run-group-elem (benchmark-group-benchmarks bs))]
   [(benchmark-one? bs) (list (run-one bs))]
   [else
    (raise-argument-error
     'run-benchmarks
     "(or/c benchmark-one? benchmark-group?)"
     bs)]))
