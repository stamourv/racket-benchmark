#lang racket

(require "util.rkt")

(provide get-past-results
         record-results)

;; path? -> (listof benchmark-result?)
(define (get-past-results file)
  (if (file-exists? file)
      (file->value file #:mode 'text)
      (error "get-past-results: file not found" file)))

;; (listof benchmark-result?) path? -> void?
(define (record-results results file)
  (write-to-file results file #:mode 'text #:exists 'truncate))
