#lang racket

(require benchmark "common.rkt")

(define (mk-optimization-group opt-type files)
  (mk-bench-group
   opt-type
   (map
    (lambda (f)
      (let ([f-path (format "oc-external/~a/~a" opt-type f)])
        (mk-shell-bench
         f
         (format "racket ~a" f-path)
         #:configure (format "raco make ~a" f-path))))
    files)))

(define benchmarks
  (map
   (lambda (otfs)
     (mk-optimization-group (car otfs) (cdr otfs)))
   optimization-type-files))

(record-results
 (run-benchmarks benchmarks (mk-bench-opts #:num-trials 30))
 results-file)
