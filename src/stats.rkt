#lang racket

(require "types.rkt")
(require math/statistics)

(provide confidence-interval
         raw-to-stats
         calculate-stats
         coeff-of-variation
         compare-benchmarks)

;; assumes random errors can be modeled by a normal distribution
;; TODO: how do we know if our errors can be modeled by a normal distribution?

;; currently only works for n ≥ 30
;; TODO: use t distribution for n < 30

;; currently only support α = .05
;; TODO: where to get precomputed table?

;; list? (num?) num? -> (num? . num?)
(define (confidence-interval vals confidence-level)
  (let ([arith-mean (mean vals)]
        [std-dev (stddev vals)]
        [n (length vals)]
        [z (if (equal? confidence-level .95)
               1.645
               (error "confidence level ≠ .95"))])
    (if (< n 30)
        (error "number of samples must be ≥ 30")
        (cons (- arith-mean (* z (/ std-dev (sqrt n))))
              (+ arith-mean (* z (/ std-dev (sqrt n))))))))

;; list? (benchmark-trial-time?) -> benchmark-trial-stats?
(define (raw-to-stats times)
  (let ([cpu (map benchmark-trial-time-cpu times)]
        [real (map benchmark-trial-time-real times)]
        [gc (map benchmark-trial-time-gc times)])
    (benchmark-trial-stats (calculate-stats cpu)
                           (calculate-stats real)
                           (calculate-stats gc))))

;; list? (num?) -> measured-value?
(define (calculate-stats vals)
  (let* ([conf-level .95]
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
      (/ (stddev vals) (mean vals))))

;; benchmark-result? benchmark-result? -> benchmark-comparison?
;; TODO: use ANOVA instead of mean of differences
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
