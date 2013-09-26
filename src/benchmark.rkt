#lang racket

(require math/statistics)
(require "types.rkt")
(require "stats.rkt")

(provide
 mk-benchmark-one
 mk-benchmark-group
 mk-benchmark-opts
 run-benchmarks
 time-one
 (struct-out benchmark-trial-time)
 record-results
 get-past-results
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
  (define (relname a b)
    (if (equal? "" a) b (string-append a "/" b)))
  (define (name-val o)
    (if (or (nothing-s? o) (nothing-s? (benchmark-opts-name o)))
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
        [time-external (opt-val benchmark-opts-time-external)])
    (benchmark-opts name gc trials itrs discard time-external)))

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
                          ))))]
               [trimmed-times
                (if (benchmark-opts-discard-first final-opts)
                    (cdr times)
                    times)]
               [stats (raw-to-stats trimmed-times)])
          (print-times (raw-to-stats trimmed-times))
          (mk-benchmark-result final-opts stats))))
    (cond
     [(benchmark-group? bs)
      (apply append (map run-group-elem (benchmark-group-benchmarks bs)))]
     [(benchmark-one? bs) (list (run-one bs))]
     [else
      (error "Invalid benchmark: expected benchmark? or benchmark-group?")]))
  (let ([results (run-benchmarks-aux benchmarks benchmark-opts)])
    (check-results results results-file-prefix)
    results))

;; list? (benchmark-result?) string? -> void
(define (check-results new-results file-prefix)
  ;; make a hash table: options -> results
  (define (mk-result-table res)
    (make-hash
     (map
      ;; alist of the form (options . results)
      (lambda (x) (cons (benchmark-result-opts x) x))
      res)))
  (let* ([new-results-table (mk-result-table new-results)]
         [past-results-table
          (mk-result-table (get-past-results file-prefix))]
         ;; comparable benchmarks are those with the same benchmark-opts?
         [comparable-benchmarks
          (set->list
           (set-intersect
            (apply set (hash-keys new-results-table))
            (apply set (hash-keys past-results-table))))]
         ;; (old-result . new-result) ... such that
         ;; (equal? (benchmark-result-opts old-result)
         ;;         (benchmark-result-opts new-result))
         [comparable-results
          (map (lambda (k)
                 (cons (hash-ref past-results-table k)
                       (hash-ref new-results-table k)))
               comparable-benchmarks)]
         [comparisons
          (map (lambda (x) (compare-benchmarks (car x) (cdr x)))
               comparable-results)]
         [filter-by-result
          (lambda (res)
            (filter (lambda (bc)
                      (equal?
                       res
                       (benchmark-comparison-result bc)))
                    comparisons))]
         [print-results
          (lambda (symb msg)
            (let ([res (filter-by-result symb)])
              (when (not (null? res))
                (displayln "")
                (displayln msg)
                (for ([i res]) 
                  (displayln (benchmark-comparison-opts i))))))])

    (print-results 'sig-improvement "Performance improvements")
    (print-results 'sig-regression "Performance regressions")
    (print-results 'not-sig "Not statistically significant")
    (record-results new-results file-prefix)))

;; list? (benchmark-trial-stats?) -> void
(define (print-times trial-times)
  (printf
   "cpu: ~a\nreal: ~a\ngc: ~a\n"
   (show-measured-value (benchmark-trial-stats-cpu trial-times))
   (show-measured-value (benchmark-trial-stats-real trial-times))
   (show-measured-value (benchmark-trial-stats-gc trial-times))))

;; procedure? -> void
(define (time-one thunk)
  (let-values ([(_ cpu real gc) (time-apply thunk '())])
    (report-time (benchmark-trial-time cpu real gc))))

(define bench-suff ".bench")

;; string? -> list? (benchmark-result?)
(define (get-past-results pref)
  (let ([file (string-append pref bench-suff)])
    (if (file-exists? file)
        (file->value file #:mode 'text)
        (list))))

;; list? (benchmark-result?) string? -> void
(define (record-results results pref)
  (let ([file (string-append pref bench-suff)])
    (write-to-file results file #:mode 'text #:exists 'truncate)))
