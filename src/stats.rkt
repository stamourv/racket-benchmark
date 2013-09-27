#lang racket

(require "types.rkt")
(require "print.rkt")
(require math/statistics)

(provide
 raw-to-stats
 compare-benchmarks
 show-measured-value
 ;; TODO: these exports are only for testing
 confidence-interval
 sample-stddev
 coeff-of-variation
 calculate-stats
 )

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

;; list? (benchmark-trial-time?) -> benchmark-trial-stats?
(define (raw-to-stats times)
  (let ([cpu (map benchmark-trial-time-cpu times)]
        [real (map benchmark-trial-time-real times)]
        [gc (map benchmark-trial-time-gc times)])
    (benchmark-trial-stats (calculate-stats cpu)
                           (calculate-stats real)
                           (calculate-stats gc))))

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
        (error "number of samples must be ≥ 30")
        (cons (- arith-mean (* z (/ std-dev (sqrt n))))
              (+ arith-mean (* z (/ std-dev (sqrt n))))))))

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

;; list? (num?) -> num?
;; stddev calculates population standard deviation, but we want sample stddev
(define (sample-stddev vals)
  (let ([n (length vals)])
    (* (stddev vals) (sqrt (/ n (- n 1))))))
