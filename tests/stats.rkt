#lang racket

(require "../src/types.rkt")
(require "../src/stats.rkt")
(require rackunit)
(require math/statistics)

(define confidence-level .95)

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

(define (test-confidence-interval)
  (for ([s samples])
    (let* ([calculated-ci (confidence-interval (tc-vals s) confidence-level)]
           [ci-lb (car calculated-ci)]
           [ci-ub (cdr calculated-ci)])
      (check-close? (tc-conf-lb s) ci-lb "lower bound")
      (check-close? (tc-conf-ub s) ci-ub "upper bound"))))

(define (test-sample-stddev)
  (for ([s samples])
    (let ([stddev (sample-stddev (tc-vals s))])
      (check-close? (tc-sample-stddev s) stddev "sample stddev"))))

(define (test-coeff-of-variation)
  (for ([s samples])
    (let ([cov (coeff-of-variation (tc-vals s))])
      (check-close? (tc-cov s) cov "coeff of var"))))

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
      (check-equal? (tc-vals s) (measured-value-samples stats)))))

(define delta .001)
(define (close? a b) (>= (+ delta (min a b)) (max a b)))
(define (check-close? a b msg) (check close? a b msg))

(test-confidence-interval)
(test-sample-stddev)
(test-coeff-of-variation)
(test-calculate-stats)
