#lang racket

  (require benchmark plot racket/match racket/vector racket/list)

  ;; list/vector sizes
  (define sizes (list 50000 100000))

  (define lists (map (lambda (i) (range i)) sizes))

  (define vectors (map list->vector lists))

  (define results
    (run-benchmarks
     ;; operations (whats)
     (list 'square-map 'self-append)
     ;; list of options (hows)
     (list
      ;; sizes (and their indices) in the sizes list
      (map cons (range (length sizes)) sizes)
      ;; implementations of operations
      (list 'vector 'list))
     ;; to run each benchmark
     (lambda (op index/size impl)
       (let ([fn
              (match (cons op impl)
                [(cons 'square-map 'vector)
                 (lambda (i) (vector-map (lambda (x) (* x x)) i))]
                [(cons 'square-map 'list)
                 (lambda (i) (map (lambda (x) (* x x)) i))]
                [(cons 'self-append 'vector)
                 (lambda (i) (vector-append i i))]
                [(cons 'self-append 'list)
                 (lambda (i) (append i i))])]
             [input (list-ref (match impl
                                ['vector vectors]
                                ['list lists])
                              (car index/size))])
         (fn input)))
     ;; don't extract time, instead time (run ...)
     #:extract-time 'delta-time
     #:num-trials 30))

  results

  (parameterize ([plot-x-ticks no-ticks])
    (plot-pict
     #:title "vectors vs lists"
     #:x-label #f
     #:y-label "normalized time"
     (render-benchmark-alts
      ;; default options
      (list (cons 0 50000) 'list)
      results
      ;; format options so we can omit the index in the size list
      #:format-opts (lambda (opts)
                      (let ([index/size (car opts)]
                            [impl (cadr opts)])
                        (format "size: ~a, ~a" (cdr index/size) impl))))))
