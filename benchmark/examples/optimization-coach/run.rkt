#lang racket

(require benchmark "common.rkt")

(define (mk-optimization-group opt-type files)
  (bench-group
   opt-type
   (map
    (lambda (f)
      (let ([f-path (format "oc-external/~a/~a" opt-type f)])
        (mk-shell-benchmark
         f
         (format "racket ~a" f-path)
         #:configure (format "raco make ~a" f-path))))
    files)))

(define benchmarks
  (bench-group
   ""
   (map
    (lambda (otfs)
      (mk-optimization-group (car otfs) (cdr otfs)))
    optimization-type-files)))

(record-results
 (run-benchmarks benchmarks (mk-bench-opts #:num-trials 30 #:discard-first #f))
 results-file)
