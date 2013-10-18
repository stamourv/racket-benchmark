#lang racket

(require "print.rkt" "types.rkt")
(require math/statistics)

(provide raw-to-stats
         normalize-br
         min-samples)

;; minimum number of samples needed to calculate confidence intervals
;; paramter to allow overriding by user during debugging
(define min-samples (make-parameter 30))

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

  ;; testcase information
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

;; raw-to-stats : (listof benchmark-trial-time?) -> benchmark-trial-stats?
(define (raw-to-stats times)
  (let ([cpu (map benchmark-trial-time-cpu times)]
        [real (map benchmark-trial-time-real times)]
        [gc (map benchmark-trial-time-gc times)])
    (benchmark-trial-stats (calculate-stats cpu)
                           (calculate-stats real)
                           (calculate-stats gc))))

;; From Lilja, appendix C.1 for n = ∞ (normal distribution)
(define default-conf-level .95)
(define default-z 1.960)

;; Calculation of confidence intervals for random errors that can
;; be modeled by the normal distribution
;; for n ≥ 30, we use the normal distribution to calculate the confidence interval
;; for n < 30, we must use the student t distribution, but currently
;; issue an error

;; confidence-internal : (Sequenceof Real) Nonnegative-Real
;;                       -> (Nonnegative-Real . Nonnegative-Real)
(define (confidence-interval vals confidence-level)
  (let ([arith-mean (mean vals)]
        [std-dev (sample-stddev vals)]
        [n (length vals)]
        [z (if (equal? confidence-level default-conf-level)
               default-z
               (error (format "confidence level ≠ ~a" default-conf-level)))])
    (if (< n (min-samples))
        (error
         (format "number of samples (~a) must be ≥ ~a. Configurable with min-samples parameter" n (min-samples)))
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

;; calculate-stats : (SequenceOf Real) -> measured-value?
(define (calculate-stats vals)
  (let* ([conf-level default-conf-level]
         [interval (confidence-interval vals conf-level)]
         [conf-lb (car interval)]
         [conf-ub (cdr interval)]
         [mean (mean vals)]
         [cov (coeff-of-variation vals)])
    (measured-value mean vals cov conf-lb conf-ub conf-level)))

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

;; coeff-of-variation : (Sequenceof Real) -> Nonnegative-Real
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

;; sample-stddev : (Sequenceof Real) -> Nonnegative-Real
(define (sample-stddev vals)
  ;; bias #t calculates sample instead of population stddev
  (stddev vals #:bias #t))

(module+ test
  (define (test-sample-stddev)
    (for ([s samples])
      (let ([stddev (sample-stddev (tc-vals s))])
        (check-close? (tc-sample-stddev s) stddev "sample stddev")))))

;; TODO: add tests for normalize-br

;; normalize-br : benchmark-result? benchmark-result? -> benchmark-result?
;; normalize sample values against mean of norm-br
(define (normalize-br norm-br br)
  (define (norm-val fn)
    (define norm-mean
      (measured-value-mean (fn (benchmark-result-trial-stats norm-br))))
    (if (zero? norm-mean)
        +inf.0
        (/ 1 norm-mean)))
  (define (samples fn)
    (measured-value-samples (fn (benchmark-result-trial-stats br))))
  (define norm-cpu-val (norm-val benchmark-trial-stats-cpu))
  (define norm-real-val (norm-val benchmark-trial-stats-real))
  (define norm-gc-val (norm-val benchmark-trial-stats-gc))
  (define normd-cpu-samples
    (map (lambda (s) (* s norm-cpu-val))
         (samples benchmark-trial-stats-cpu)))
  (define normd-real-samples
    (map (lambda (s) (* s norm-real-val))
         (samples benchmark-trial-stats-real)))
  (define normd-gc-samples
    (map (lambda (s) (* s norm-gc-val))
         (samples benchmark-trial-stats-gc)))
  (benchmark-result
   (benchmark-result-opts br)
   (raw-to-stats (map benchmark-trial-time
                      normd-cpu-samples
                      normd-real-samples
                      normd-gc-samples))))

(module+ test
  (test-confidence-interval)
  (test-sample-stddev)
  (test-coeff-of-variation)
  (test-calculate-stats))
