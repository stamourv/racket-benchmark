#lang racket

(require benchmark "common.rkt")

(define (mk-optimization-group opt-type files)
  (bgroup
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
  (bgroup
   ""
   (map
    (lambda (otfs)
      (mk-optimization-group (car otfs) (cdr otfs)))
    optimization-type-files)))

(record-results
 (run-benchmarks benchmarks (bopts #:num-trials 30 #:discard-first #f))
 results-file)
