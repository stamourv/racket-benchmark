#lang racket

(require benchmark "common.rkt")

(define (make-optimization-group opt-type files)
  (parameterize ([num-trials 30])
    (make-bench-group
     opt-type
     (map
      (lambda (f)
        (let ([f-path (format "oc-external/~a/~a" opt-type f)])
        (make-shell-bench
         f
         (format "racket ~a" f-path)
         #:configure (format "raco make ~a" f-path))))
      files))))

(define benchmarks
  (map
   (lambda (otfs)
     (make-optimization-group (car otfs) (cdr otfs)))
   optimization-type-files))

(record-results (run-benchmarks benchmarks) results-file)
