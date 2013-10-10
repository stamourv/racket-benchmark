#lang racket

(provide maybe-mkdir)

;; maybe-mkdir : path-string? -> void?
(define (maybe-mkdir d)
  (unless (directory-exists? d)
    (make-directory d)))
