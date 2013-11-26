#lang racket

(require "types.rkt"
         "stats.rkt"
         "print.rkt"
         "time.rkt")
(require math/statistics)

(provide run-benchmarks)

;; run-benchmarks : (listof benchmark-one?)
;;                  -> (listof benchmark-result?)
(define (run-benchmarks bs)
  ;; run a benchmark-one?
  (define (run-one b)
    (let*
        ;; compute final options, filling in with default values as needed
        ([final-opts (benchmark-one-opts b)]
         [final-name (benchmark-one-name b)]
         [itrs-per-trial (benchmark-opts-itrs-per-trial final-opts)]
         [adjusted-num-trials
          (if (benchmark-opts-discard-first final-opts)
              (+ 1 (benchmark-opts-num-trials final-opts))
              (benchmark-opts-num-trials final-opts))])
      (displayln
       (format "Running benchmark: ~a, ~a trials, ~a runs per trial"
               final-name
               (benchmark-opts-num-trials final-opts)
               (benchmark-opts-itrs-per-trial final-opts)))
      (let*
          ([times
            ;; for each trial
            (for/list ([i adjusted-num-trials])
              (begin
                (when (benchmark-opts-gc-between final-opts)
                  (collect-garbage)
                  (collect-garbage)
                  (collect-garbage))
                (cond
                 ;; we will do the timing using time-apply
                 [(not (benchmark-opts-manual-report-time final-opts))
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
        (make-bench-result final-name final-opts stats))))
  (map run-one bs))
