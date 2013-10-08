#lang racket

(require "../src/benchmark.rkt" plot)

(module typed typed/racket
  (require racket/unsafe/ops)
  (require/typed "../src/time.rkt"
                 [time-internal ((-> Any) -> Void)])

  
  (provide complete-thunks partial-thunks)
  (define n 1000000)
  (define m 10000)
  (define l (make-list 10000 "a"))
  (define l2 (build-list 1000000 (lambda: ([x : Integer]) x)))
  
  (define complete-thunks
    (list
     (lambda () (time-internal
                 (lambda () (for ([i (in-range n)]) (unsafe-fx+ i 1)))))
     (lambda () (time-internal
                 (lambda () (for ([i (in-range m)]) (format "eh~a" i)))))
     (lambda () (time-internal
                 (lambda () (for ([i (in-list l)]) (format "eh~a" i)))))
     (lambda () (time-internal
                 (lambda () (for ([i (in-list l2)]) (unsafe-fx+ i 1)))))))
  
  (define partial-thunks
    (list
     (lambda () (time-internal (lambda () (for ([i n]) (unsafe-fx+ i 1)))))
     (lambda () (time-internal (lambda () (for ([i m]) (format "eh~a" i)))))
     (lambda () (time-internal (lambda () (for ([i l]) (format "eh~a" i)))))
     (lambda () (time-internal (lambda () (for ([i l2]) (unsafe-fx+ i 1))))))))

(require (submod "." typed))

(define names (list "unsafe-fx+ int" "format int" "format list" "unsafe-fx+ list"))

(define benches
  (mk-benchmark-group
   ""
   (list
    (mk-benchmark-group
     "partial"
     (map mk-benchmark-one names partial-thunks))
    (mk-benchmark-group
     "complete"
     (map mk-benchmark-one names complete-thunks)))))

(define results
  (run-benchmarks
   benches
   #:benchmark-opts
   (mk-benchmark-opts #:gc-between #f #:num-trials 31 #:itrs-per-trial 10)))

(parameterize ([plot-x-ticks no-ticks])
  (plot-file
   (render-benchmark-alts (list "complete" "partial") results "partial")
   "optimization-coach.pdf"))
