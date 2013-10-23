#lang racket

(require benchmark)

(provide sleep-external-bench)

(define (sleep-external-bench n)
  (mk-shell-bench
   (format "sleep ~a" n)
   (format "( /usr/bin/time -p sleep ~a ) 2>&1" n)
   #:extract-result linux-time-extract-result))
