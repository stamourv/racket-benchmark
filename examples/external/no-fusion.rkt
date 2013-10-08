#lang racket

(define (add1 n) (+ n 1))
(define (square n) (* n n))

(define big-list (for/list ([i 100000])))
(time
 (map add1 (map square (map sqrt big-list))))

