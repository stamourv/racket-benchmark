#lang racket

(require math/statistics)
(require "types.rkt")
(require "stats.rkt")
(require "print.rkt")
(require "time.rkt")
(require "results.rkt")
(require "external.rkt")
(require "plot.rkt")

(provide
 mk-benchmark-one
 mk-shell-benchmark
 mk-benchmark-group
 mk-benchmark-opts
 run-benchmarks
 time-internal
 plot-benchmark-result
 )

;; this module implements a simple benchmarking library
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
    (let ([filtered-opts (filter-nothing (map fn (filter-nothing (list o1 o2))))])
      (if (null? filtered-opts)
          nothing
          (car filtered-opts))))
  (let ([name (relname (name-val o2) (name-val o1))]
        [gc (opt-val benchmark-opts-gc-between-each)]
        [trials (opt-val benchmark-opts-num-trials)]
        [itrs (opt-val benchmark-opts-itrs-per-trial)]
        [discard (opt-val benchmark-opts-discard-first)]
        [time-external (opt-val benchmark-opts-time-external)]
        [plot-file (opt-val benchmark-opts-plot-file)])
    (benchmark-opts name gc trials itrs discard time-external plot-file)))

(define (append-default-opts o) (append-opts o default-opts))

;; (benchmark-one? or benchmark-group?) -> list? (benchmark-result?)
;; TODO: clean up
(define (run-benchmarks benchmarks
                        #:benchmark-opts [benchmark-opts nothing]
                        #:results-file-prefix [results-file-prefix "default"])
  (define (run-benchmarks-aux bs opts)
    ;; run a benchmark (benchmark-one? or benchmark-group?)
    ;; from benchmark-group? bs, remembering the name and
    ;; benchmark-options? of the bs
    (define (run-group-elem e)
      (run-benchmarks-aux
       e
       (append-opts (benchmark-group-opts bs) opts)))
    ;; run a benchmark-one?
    (define (run-one b)
      (let ([final-opts
             (append-default-opts (append-opts (benchmark-one-opts b) opts))])
        (displayln
         (format "Running benchmark: ~a, ~a trials, ~a runs per trial"
                 (benchmark-opts-name final-opts)
                 (benchmark-opts-num-trials final-opts)
                 (benchmark-opts-itrs-per-trial final-opts)))
        (let* ([times
                ;; for each trial
                (for/list ([i (benchmark-opts-num-trials final-opts)])
                  (begin
                    (when (benchmark-opts-gc-between-each final-opts)
                      (collect-garbage)
                      (collect-garbage)
                      (collect-garbage))
                    (cond
                     [(benchmark-opts-time-external final-opts)
                      (let-values
                          ([(_ cpu real gc)
                            (time-apply
                             (lambda ()
                               (for ([j (benchmark-opts-itrs-per-trial final-opts)])
                                 ((benchmark-one-thunk b))))
                             '())])
                        (benchmark-trial-time cpu real gc))]
                     [else
                      ((benchmark-one-thunk b))
                      (receive-time)])))]
               [trimmed-times
                (if (benchmark-opts-discard-first final-opts)
                    (cdr times)
                    times)]
               [stats (raw-to-stats trimmed-times)])
          (print-times (raw-to-stats trimmed-times))
          (mk-benchmark-result final-opts stats))))
    (cond
     [(benchmark-group? bs)
      (append-map run-group-elem (benchmark-group-benchmarks bs))]
     [(benchmark-one? bs) (list (run-one bs))]
     [else
      (error "Invalid benchmark: expected benchmark? or benchmark-group?")]))
  (let ([results (run-benchmarks-aux benchmarks benchmark-opts)])
    (check-results results results-file-prefix)
    results))


