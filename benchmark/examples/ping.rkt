#lang racket

(require plot benchmark)

(define ping-num-trials 5)

(define (ping count)
  (mk-shell-bench
   "ping"
   (format "ping -c ~a ccs.neu.edu" count)
   #:extract-result extract-time
   #:gc-between #f
   #:num-trials 30
   #:discard-first #f))

(define (extract-time str)
  (displayln str)
  (let* ([m (regexp-match
             #rx#"([a-z/]+) = ([0-9.]+)/([0-9.]+)/.*" str)])
    (if (not m)
        (error (format "Could not parse time output: ~a" str))
        (benchmark-trial-time 0 (bytes->number (cadddr m)) 0))))

(define (bytes->number b)
  (string->number (bytes->string/latin-1 b)))

(define benches
  (map
   (lambda (n) (mk-bench-group (format "trial ~a" n) (list (ping 2))))
   (for/list ([i (in-range 0 ping-num-trials)]) i)))

(define results (run-benchmarks benches))

(parameterize ([plot-x-ticks no-ticks])
  (plot-file
   #:title "multiple ping trials"
   #:y-label "normalized time"
   #:x-label #f
   (render-benchmark-alts
    (map (lambda (n) (format "trial ~a" n))
         (for/list ([i (in-range 0 ping-num-trials)]) i))
    "trial 0"
    results)
   "ping.pdf"))
