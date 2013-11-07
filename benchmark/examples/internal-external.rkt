#lang racket

(require plot benchmark)

(define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))

(define (fib-internal n)
  (define (do-fib) (for ([i 500]) (fib n)))
  (do-fib)
  (time-internal do-fib))

(define (make-fib-bench n fn)
  (bench-one (format "fib ~a" n) (fn n)))

(define fib-inputs (list 19 20 21))

(define fib-internal-group
  (parameterize ([manual-report-time #t]
                 [gc-between #f]
                 [itrs-per-trial 1])
    (make-bench-group
     "internals"
     (map (lambda (n)
            (make-fib-bench n fib-internal)) fib-inputs))))

(define fib-external-group
  (parameterize ([gc-between #f])
    (make-bench-group
     "externals"
     (map (lambda (m) (make-fib-bench m fib)) fib-inputs))))

;; note: can't do something like:
;; (parameterize ([gc-between #f])
;;   fib-external-group)

(define results
  (run-benchmarks (list fib-internal-group fib-external-group)))

(parameterize ([plot-x-ticks no-ticks])
  (plot-file
   #:title "internal vs external timing"
   #:x-label #f
   #:y-label "normalized time"
   (render-benchmark-alts (list "internals" "externals") "externals" results)
   "internal-external.pdf"))
