#lang typed/racket

(require math/base
         math/flonum
         math/distributions
         math/statistics)

(provide parametric-bootstrap
         nonparametric-bootstrap
         nonparametric2d-bootstrap
         bootstrap-percentile-conf
         bootstrap-basic-conf
         bootstrap-bca-conf)

;; ---------------------------------------------------------------------------------------------------
;; Procedures for getting bootstrap samples

(: parametric-bootstrap
   (All (A B) (case-> (((Listof A) -> B) (distribution A A) Integer -> (Listof B))
                      (((Listof A) -> B) (distribution A A) Integer Integer -> (Listof B)))))
;; Returns m samples of the statistic computed by f, using n samples. Use this when you know the
;; distribution of the data.
(define (parametric-bootstrap f d n [m 1999])
  (build-list m (λ _ (f (sample d n)))))

(: nonparametric-bootstrap
   (All (A B) (case-> (((Listof A) -> B) (Listof A) -> (Listof B))
                      (((Listof A) -> B) (Listof A) Integer -> (Listof B)))))
;; Returns m samples of the statistic computed by f, by resampling xs with replacement.
(define (nonparametric-bootstrap f xs [m 1999])
  (parametric-bootstrap f (discrete-dist xs) (length xs) m))

(: nonparametric2d-bootstrap
   (All (A B C) (case-> (((Listof A) (Listof B) -> C) (Listof A) (Listof B) -> (Listof C))
                        (((Listof A) (Listof B) -> C) (Listof A) (Listof B) Integer -> (Listof C)))))
;; Returns m samples of the statistic computed by f, by resampling xs and ys with replacement,
;; independently. For the distributions of a function of two independent random variables, this
;; should produce bootstrap samples with less error.
;; Do not use if xs and ys are correlated!
(define (nonparametric2d-bootstrap f xs ys [m 1999])
  (define n (max (length xs) (length ys)))
  (define d1 (discrete-dist xs))
  (define d2 (discrete-dist ys))
  (build-list m (λ _ (f (sample d1 n) (sample d2 n)))))

;; ---------------------------------------------------------------------------------------------------

(: jackknife-samples (All (A) (((Listof A) -> A) (Listof A) -> (Listof A))))
;; Compute a list of leave-one-out estimators, using a functional zipper. Used by bootstrap-bca-conf.
(define (jackknife-samples f xs)
  (let: loop ([xs-pre : (Listof A)  empty] [x : A  (first xs)] [xs-post : (Listof A)  (rest xs)])
    (cond [(empty? xs-post)  (list (f xs-pre))]
          [else  (cons (f (append xs-pre xs-post))
                       (loop (cons x xs-pre) (first xs-post) (rest xs-post)))])))

;; ---------------------------------------------------------------------------------------------------
;; Procedures for computing one side of a confidence interval from bootstrap samples

(: bootstrap-percentile-conf ((Listof Real) Real -> Real))
;; Fast, simple, easy to understand. Invariant under monotone transformations. Error can be high if
;; the data's distribution isn't symmetric.
(define (bootstrap-percentile-conf ^θ*s α)
  (quantile α < ^θ*s))

(: bootstrap-basic-conf (((Listof Real) -> Real) (Listof Real) (Listof Real) Real -> Real))
;; Great for the sample median. Error can be quite high if the bootstrap distribution differs
;; significantly from the statistic's. Might violate parameter constraints such as θ > 0.
(define (bootstrap-basic-conf f xs ^θ*s α)
  (define ^θ (f xs))
  (- ^θ (quantile (- 1 α) < (map (λ: ([^θ* : Real]) (- ^θ* ^θ)) ^θ*s))))

(: bootstrap-bca-conf (((Listof Real) -> Real) (Listof Real) (Listof Real) Real -> Real))
;; Same advantages as percentile method, but generally has smaller error. Can be erratic when α is
;; much smaller than 0.025.
(define (bootstrap-bca-conf f xs ^θ*s α)
  (define ^θ (f xs))
  (define p (count (λ: ([^θ* : Real]) (^θ* . < . ^θ)) ^θ*s))
  (define b (flnormal-inv-cdf 0.0 1.0 (fl (/ p (length ^θ*s))) #f #f))
  (define ^θis (jackknife-samples f xs))
  (define ~θ (mean ^θis))
  (define den (real-part (expt (sum (map (λ: ([^θi : Real]) (sqr (- ~θ ^θi))) ^θis)) 3/2)))
  (define s
    (cond [(zero? den)  0]
          [else
           (define num (sum (map (λ: ([^θi : Real]) (expt (- ~θ ^θi) 3)) ^θis)))
           (define a (* 1/6 (/ num den)))
           (define z1mα (flnormal-inv-cdf 0.0 1.0 (- 1.0 α) #f #f))
           (/ (- z1mα b) (+ 1 (* a (- z1mα b))))]))
  (define β (flnormal-cdf 0.0 1.0 (fl (- b s)) #f #f))
  (quantile β < ^θ*s))
