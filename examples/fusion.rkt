#lang racket

(require "../src/benchmark.rkt")
(require plot (for-syntax syntax/parse))

(define (add1 n) (+ n 1))
(define (square n) (* n n))

(define list-sizes (list 1000 5000 10000))
(define list-sizes-strs (map (lambda (n) (format "~a" n)) list-sizes))

(define sample-lists
  (for/list ([i list-sizes])
    (for/list ([j i]) j)))

(define-syntax (fuse stx)
  (syntax-parse stx
    [(_ l) #'(map (lambda (n) (add1 (square (sqrt n)))) l)]))

(define-syntax (no-fuse stx)
  (syntax-parse stx
    [(_ l) #'(map add1 (map square (map sqrt l)))]))

(define benches
  (mk-bgroup
   ""
   (list
    (mk-bgroup
     "fusion"
     (map
      (lambda (n l) (mk-b1 n (thunk (fuse l))))
      list-sizes-strs sample-lists))
    (mk-bgroup
     "no-fusion"
     (map
      (lambda (n l) (mk-b1 n (thunk (no-fuse l))))
      list-sizes-strs sample-lists)))))

(define results
  (run-benchmarks
   benches
   (bopts #:gc-between #f #:itrs-per-trial 200 #:num-trials 50)))

(parameterize ([plot-x-ticks no-ticks])
  (plot-file
   (render-benchmark-alts (list "fusion" "no-fusion") results "fusion")
   "fusion-no-fusion.pdf"))
