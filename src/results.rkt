#lang racket

(require "types.rkt")
(require "stats.rkt")

(provide
 check-results
 record-results
 get-past-results
 )

;; list? (benchmark-result?) string? -> void
(define (check-results new-results file-prefix)
  ;; make a hash table: options -> results
  (define (mk-result-table res)
     (for/hash ([h res])
      ;; alist of the form (options . results)
      (values (benchmark-result-opts h) h)))
  (let* ([new-results-table (mk-result-table new-results)]
         [past-results-table
          (mk-result-table (get-past-results file-prefix))]
         ;; comparable benchmarks are those with the same benchmark-opts?
         [comparable-benchmarks
          (set->list
           (set-intersect
            (list->set (hash-keys new-results-table))
            (list->set (hash-keys past-results-table))))]
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
