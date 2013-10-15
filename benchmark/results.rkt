#lang racket

(require "util.rkt")

(provide get-past-results
         record-results)

(define bench-dir "benches/")

;; string? -> (listof benchmark-result?)
(define (get-past-results file-base)
  (let ([file (string-append bench-dir file-base)])
    (if (file-exists? file)
        (file->value file #:mode 'text)
        (list))))

;; (listof benchmark-result?) string? -> void?
(define (record-results results file-base)
  (let ([file (string-append bench-dir file-base)])
    (maybe-mkdir bench-dir)
    (write-to-file results file #:mode 'text #:exists 'truncate)))
