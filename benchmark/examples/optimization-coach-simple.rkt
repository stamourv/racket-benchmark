#lang racket

(module typed typed/racket
  (require racket/unsafe/ops)
  (require/typed benchmark
                 [time-internal ((-> Any) -> Void)])

    (provide complete-thunks partial-thunks names)

    (define n 1000000)
    (define m 10000)
    (define l (make-list 10000 "a"))
    (define l2 (build-list 1000000 (lambda: ([x : Integer]) x)))

    (define names
      (list "unsafe-fx+ int" "format int" "format list" "unsafe-fx+ list"))

    (define complete-thunks
      (list
       (thunk (time-internal (thunk (for ([i (in-range n)]) (unsafe-fx+ i 1)))))
       (thunk (time-internal (thunk (for ([i (in-range m)]) (format "eh~a" i)))))
       (thunk (time-internal (thunk (for ([i (in-list l)]) (format "eh~a" i)))))
       (thunk (time-internal (thunk (for ([i (in-list l2)]) (unsafe-fx+ i 1)))))))

    (define partial-thunks
      (list
       (thunk (time-internal (thunk (for ([i n]) (unsafe-fx+ i 1)))))
       (thunk (time-internal (thunk (for ([i m]) (format "eh~a" i)))))
       (thunk (time-internal (thunk (for ([i l]) (format "eh~a" i)))))
       (thunk (time-internal (thunk (for ([i l2]) (unsafe-fx+ i 1))))))))

(require (submod "." typed) benchmark plot)

(define benches
  (mk-bgroup
   ""
   (list
    (mk-bgroup "partial" (map mk-b1 names partial-thunks))
    (mk-bgroup "complete" (map mk-b1 names complete-thunks)))))

(define results
  (run-benchmarks
   benches
   (bopts #:gc-between #f #:num-trials 31 #:itrs-per-trial 10)))

(parameterize ([plot-x-ticks no-ticks])
  (plot-file
   #:title "optimization coach"
   #:y-label "normalized time"
   #:x-label #f
   (render-benchmark-alts (list "complete" "partial") results "partial")
   "optimization-coach-simple.pdf"))
