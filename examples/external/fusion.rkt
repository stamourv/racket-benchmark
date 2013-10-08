#lang racket

(define (add1 n) (+ n 1))
(define (square n) (* n n))

(define big-list (for/list ([i 100000])))
(time
 (map (lambda (n) (add1 (square (sqt n)))) big-list))

