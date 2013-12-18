#lang typed/racket

(require plot/typed
         math/base
         math/flonum
         math/distributions
         math/statistics
         "bootstrap-ci.rkt")

;; Examples

;; xs: simulated non-jit times
(define xs (map (λ: ([x : Real]) (+ x 10)) (sample (binomial-dist 20 0.75) 20)))
;; ys: simulated jit times
(define ys (map (λ: ([x : Real]) (+ x 10)) (sample (binomial-dist 10 0.5) 20)))

(define ys/xs (map / ys xs))

(plot (density xs))
(plot (density ys))
(plot (density ys/xs))

(: mean-quotient ((Listof Real) (Listof Real) -> Real))
;; The statistic of interest
(define (mean-quotient ys xs)
  (mean (map / ys xs)))

(define ^θ*s0 (parametric-bootstrap mean (normal-dist (mean ys/xs) (stddev ys/xs)) (length ys/xs)))
(define ^θ*s1 (nonparametric-bootstrap mean ys/xs))
(define ^θ*s2 (nonparametric2d-bootstrap mean-quotient ys xs))

(printf "Confidence intervals from parametric bootstrap~n")
(list (bootstrap-percentile-conf ^θ*s0 0.025)
      (bootstrap-percentile-conf ^θ*s0 0.975))
(list (bootstrap-basic-conf mean ys/xs ^θ*s0 0.025)
      (bootstrap-basic-conf mean ys/xs ^θ*s0 0.975))
(list (bootstrap-bca-conf mean ys/xs ^θ*s0 0.025)
      (bootstrap-bca-conf mean ys/xs ^θ*s0 0.975))
(newline)

(printf "Confidence intervals from nonparametric bootstrap~n")
(list (bootstrap-percentile-conf ^θ*s1 0.025)
      (bootstrap-percentile-conf ^θ*s1 0.975))
(list (bootstrap-basic-conf mean ys/xs ^θ*s1 0.025)
      (bootstrap-basic-conf mean ys/xs ^θ*s1 0.975))
(list (bootstrap-bca-conf mean ys/xs ^θ*s1 0.025)
      (bootstrap-bca-conf mean ys/xs ^θ*s1 0.975))
(newline)

(printf "Confidence intervals from parametric 2D (independent) bootstrap~n")
(list (bootstrap-percentile-conf ^θ*s2 0.025)
      (bootstrap-percentile-conf ^θ*s2 0.975))
(list (bootstrap-basic-conf mean ys/xs ^θ*s2 0.025)
      (bootstrap-basic-conf mean ys/xs ^θ*s2 0.975))
(list (bootstrap-bca-conf mean ys/xs ^θ*s2 0.025)
      (bootstrap-bca-conf mean ys/xs ^θ*s2 0.975))
(newline)
