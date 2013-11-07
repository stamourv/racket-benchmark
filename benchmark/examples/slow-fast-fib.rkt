#lang racket

(require benchmark)

;; format name
(define (fmt n) (format "fib ~a" n))

(define (slow-fib n)
  (if (> 2 n) n (+ (slow-fib (- n 1)) (slow-fib (- n 2)))))

(define (fast-fib n)
  (define vec (make-vector (+ n 1)))
   (define (vec-fib i)
     (when (<= i n)
       (vector-set!
        vec
        i
        (if (> 2 i)
            i
            (+ (vector-ref vec (- i 1)) (vector-ref vec (- i 2)))))
       (vec-fib (+ 1 i))))
   (vec-fib 0)
   (vector-ref vec n))

(define inputs (for/list ([i (in-range 20 25)]) i))

(define benches
  (parameterize ([gc-between #f])
    (list
     (mk-bench-group
      "slow (1 itr per trial)"
      (map
       (lambda (i)
         (parameterize ([itrs-per-trial 1])
           (bench-one (fmt i) (slow-fib i))))
       inputs))
     (mk-bench-group
      "fast (10000 itrs per trial)"
      (map
       (lambda (i)
         (parameterize ([itrs-per-trial 10000])
           (bench-one (fmt i) (fast-fib i))))
       inputs)))))

(run-benchmarks benches)
