#lang racket

(require "types.rkt")
(require "print.rkt")
(require math/statistics)

(provide
 raw-to-stats
 compare-benchmarks
 show-measured-value)

;; assumes random errors can be modeled by a normal distribution
;; TODO: how do we know if our errors can be modeled by a normal distribution?
;; TODO: should we normalize in the case thta our errors don't follow a
;;       normal distribution?

;; currently only works for n ≥ 30
;; TODO: use t distribution for n < 30

;; currently only support α = 1 - default-conf-level
;; TODO: where to get precomputed table?

;; Confidence intervals and comparison of benchmarks through difference of
;; means were described in 'Measuring Computer Performance: A Practitioner's Guide
;; by D. J. Lilja. A more rigorous comparison of benchmarks (which will replace
;; difference of means is Analysis of Variance [5.2.1])

;; @article{3371776,
;;          author = {D. J. Lilja},
;;          title = {{Measuring Computer Performance: A Practitioner''s Guide}},
;;          year = {2000},
;;          masid = {3371776}
;;          }


(module+ test
  (require rackunit)

  (struct tc
    (vals
     conf-lb
     conf-ub
     sample-stddev
     cov
     )
    #:transparent
    )
  
  (define samples
    (list
     (tc (for/list ([i 30]) 0) 0 0 0 +inf.0)
     (tc (for/list ([i 30]) i) 11.3497 17.6502 8.8034 .6071)
     (tc (for/list ([i 50]) i) 20.4594 28.5406 14.5774 .5950)
     (tc (for/list ([i (in-range 50 100)]) i) 70.4594 78.5406 14.5774 .1957)
     (tc (for/list ([i 100]) i) 43.8139 55.1861 29.01149 .5861)
     ))
  
  (define delta .001)
  (define (close? a b) (>= (+ delta (min a b)) (max a b)))
  (define (check-close? a b msg) (check close? a b msg)))

;; list? (benchmark-trial-time?) -> benchmark-trial-stats?
(define (raw-to-stats times)
  (let ([cpu (map benchmark-trial-time-cpu times)]
        [real (map benchmark-trial-time-real times)]
        [gc (map benchmark-trial-time-gc times)])
    (benchmark-trial-stats (calculate-stats cpu)
                           (calculate-stats real)
                           (calculate-stats gc))))

(module+ test
  (define (test-calculate-stats)
    (for ([s samples])
      (let ([stats (calculate-stats (tc-vals s))])
        (for ([i (list measured-value-coeff-of-var
                       measured-value-conf-lb
                       measured-value-conf-ub)]
              [j (list tc-cov
                       tc-conf-lb
                       tc-conf-ub)])
          (check-close? (j s) (i stats) "calculate stats"))
        (check-equal? (tc-vals s) (measured-value-samples stats))))))

;; currently using 'mean of differences' as described in 5.1.1 of Lilja
;; as equal benchmark-opts implies corresponding measurements

;; TODO: use ANOVA instead of mean of differences
;; TODO: add test cases

;; benchmark-result? benchmark-result? -> benchmark-comparison?
(define (compare-benchmarks br1 br2)
  (let* ([samples (lambda (b)
                    (measured-value-samples
                     (benchmark-trial-stats-real
                      (benchmark-result-trial-stats b))))]
         [diffs (map (lambda (x y) (- x y)) (samples br1) (samples br2))]
         [diff-stats (calculate-stats diffs)]
         [sig (cond [(> (measured-value-conf-lb diff-stats) 0)
                     'sig-improvement]
                    [(< (measured-value-conf-ub diff-stats) 0)
                     'sig-regression]
                    [else 'not-sig])])
    (mk-benchmark-comparison sig (benchmark-result-opts br1))))

;; From Lilja, appendix C.1 for n = ∞ (normal distribution)
(define default-conf-level .95)
(define default-z 1.960)

;; Calculation of confidence intervals for random errors that can
;; be modeled by the normal distribution
;; for n ≥ 30, we use the normal distribution to calculate the confidence interval
;; for n < 30, we must use the student t distribution
;; From Lilja 4.4.1

;; list? (num?) num? -> (num? . num?)
(define (confidence-interval vals confidence-level)
  (let ([arith-mean (mean vals)]
        [std-dev (sample-stddev vals)]
        [n (length vals)]
        [z (if (equal? confidence-level default-conf-level)
               default-z
               (error (format "confidence level ≠ ~a" default-conf-level)))])
    (if (< n 30)
        (error (format "number of samples (~a) must be ≥ 30" n))
        (cons (- arith-mean (* z (/ std-dev (sqrt n))))
              (+ arith-mean (* z (/ std-dev (sqrt n))))))))

(module+ test
  (define (test-confidence-interval)
    (for ([s samples])
      (let* ([calculated-ci (confidence-interval (tc-vals s) default-conf-level)]
             [ci-lb (car calculated-ci)]
             [ci-ub (cdr calculated-ci)])
        (check-close? (tc-conf-lb s) ci-lb "lower bound")
        (check-close? (tc-conf-ub s) ci-ub "upper bound")))))

;; list? (num?) -> measured-value?
(define (calculate-stats vals)
  (let* ([conf-level default-conf-level]
         [interval (confidence-interval vals conf-level)]
         [conf-lb (car interval)]
         [conf-ub (cdr interval)]
         [mean (mean vals)]
         [cov (coeff-of-variation vals)])
    (measured-value mean vals cov conf-lb conf-ub conf-level)))

;; list? (num?) -> num?
(define (coeff-of-variation vals)
  (if (= 0 (mean vals))
      ;; TODO: what is the appropriate return value?
      +inf.0
      (/ (sample-stddev vals) (mean vals))))

(module+ test
  (define (test-coeff-of-variation)
    (for ([s samples])
      (let ([cov (coeff-of-variation (tc-vals s))])
        (check-close? (tc-cov s) cov "coeff of var")))))

;; list? (num?) -> num?
;; stddev calculates population standard deviation, but we want sample stddev
(define (sample-stddev vals) (stddev vals #:bias #t))

(module+ test
  (define (test-sample-stddev)
    (for ([s samples])
      (let ([stddev (sample-stddev (tc-vals s))])
        (check-close? (tc-sample-stddev s) stddev "sample stddev")))))

(module+ test
  (test-confidence-interval)
  (test-sample-stddev)
  (test-coeff-of-variation)
  (test-calculate-stats))
