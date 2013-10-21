#lang racket

;; Adapted (mostly copied) from https://gist.github.com/samth/7088570,
;; which was in response to '[racket] Small and short-lived data structures'
;; on users@racket-lang.org

(require racket/unsafe/ops racket/performance-hint)

(define (loop0 n)
  (for/sum ([i (in-range n)])
    (define i+1 (unsafe-fx+ 1 i))
    (define g (+ i i+1))
    (define l (- i i+1))
    (+ g l)))

(begin-encourage-inline
 (define (f1 x y)
   (cons (+ x y) (- x y)))
 (define (g1 x)
   (+ (unsafe-car x) (unsafe-cdr x))))

(define (loop1 n)
  (for/sum ([i (in-range n)])
    (g1 (f1 i (unsafe-fx+ 1 i)))))

(begin-encourage-inline
 (define (f2 x y)
   (values (+ x y) (- x y)))
 (define (g2 a b) (+ a b)))

(define (loop2 n)
  (for/sum ([i (in-range n)])
    (call-with-values (lambda () (f2 i (unsafe-fx+ 1 i))) g2)))

(begin-encourage-inline
 (define (f3 p x y)
   (unsafe-set-mcar! p (+ x y))
   (unsafe-set-mcdr! p (- x y))
   p)
 (define (g3 p) (+ (unsafe-mcar p) (unsafe-mcdr p))))

(define (loop3 n)
  (define p (mcons #f #f))
  (for/sum ([i (in-range n)])
    (g3 (f3 p i (unsafe-fx+ 1 i)))))

(define K 10000000)

;; begin usage of benchmark library

(require plot benchmark (for-syntax benchmark))

(define benches
  (list
   (bench-one "direct" (loop0 K))
   (bench-one "cons" (loop1 K))
   (bench-one "values" (loop2 K))
   (bench-one "mcons" (loop3 K))))

(define results
  (run-benchmarks benches (mk-bench-opts
                           #:itrs-per-trial 1
                           #:num-trials 30)))

(parameterize ([plot-x-ticks no-ticks])
  (plot-file
   #:title "Small Data Structures"
   #:x-label #f
   #:y-label "normalized time"
   (render-benchmark-alts
    (list "direct" "cons" "values" "mcons")
    "direct"
    results)
   "small-data-structures.pdf"))
