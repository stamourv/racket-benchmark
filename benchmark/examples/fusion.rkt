#lang racket

(require plot benchmark (for-syntax syntax/parse))

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
  (parameterize ([gc-between #f]
                 [itrs-per-trial 200]
                 [num-trials 50])
    (make-bench-group
     ""
     (list
      (make-bench-group
       "fusion"
       (map
        (lambda (n l) (make-bench-one n (thunk (fuse l))))
        list-sizes-strs sample-lists))
      (make-bench-group
       "no-fusion"
       (map
        (lambda (n l) (make-bench-one n (thunk (no-fuse l))))
        list-sizes-strs sample-lists))))))

(define results (run-benchmarks benches))

(parameterize ([plot-x-ticks no-ticks])
  (plot-file
   #:title "map fusion"
   #:x-label "list size"
   #:y-label "normalized time"
   (render-benchmark-alts (list "fusion" "no-fusion") "fusion" results)
   "fusion-no-fusion.pdf"))
