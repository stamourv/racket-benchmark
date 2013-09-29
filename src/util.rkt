#lang racket

(provide
 maybe-mkdir)

(define (maybe-mkdir d)
  (unless (directory-exists? d)
    (make-directory d)))
