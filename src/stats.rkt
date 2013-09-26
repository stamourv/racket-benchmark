#lang racket

(require "types.rkt")
(require math/statistics)

(provide
 raw-to-stats
 compare-benchmarks
 show-measured-value
 ;; only for testing
 confidence-interval
 sample-stddev
 coeff-of-variation
 calculate-stats
 )

;; assumes random errors can be modeled by a normal distribution
;; TODO: how do we know if our errors can be modeled by a normal distribution?

;; currently only works for n ≥ 30
;; TODO: use t distribution for n < 30

;; currently only support α = 1 - default-conf-level
;; TODO: where to get precomputed table?

;; list? (benchmark-trial-time?) -> benchmark-trial-stats?
(define (raw-to-stats times)
  (let ([cpu (map benchmark-trial-time-cpu times)]
        [real (map benchmark-trial-time-real times)]
        [gc (map benchmark-trial-time-gc times)])
    (benchmark-trial-stats (calculate-stats cpu)
                           (calculate-stats real)
                           (calculate-stats gc))))

;; benchmark-result? benchmark-result? -> benchmark-comparison?
;; TODO: use ANOVA instead of mean of differences
;; TODO: add test cases
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

;; measured-value? -> string?
(define (show-measured-value mv)
  (format "mean: ~a, coeff-of-var: ~a"
          (exact->inexact (measured-value-mean mv))
          (exact->inexact (measured-value-coeff-of-var mv))))

(define default-conf-level .95)
(define default-z 1.960)

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
;; stddev calculates population standard deviation
(define (sample-stddev vals)
  (let ([n (length vals)])
    (* (stddev vals) (sqrt (/ n (- n 1))))))
