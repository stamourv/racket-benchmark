#lang racket

(require plot benchmark)

(define hand-optimized-files
  (list
   "binarytrees.rkt"
   "cantor.rkt"
   "heapsort.rkt"
   "mandelbrot.rkt"
   "moments.rkt"
   "nbody.rkt"))

(define all-files
  (append hand-optimized-files
          (list "maze.rkt"
                "ray-tracer.rkt"
                "pseudoknot.rkt"
                "video.rkt")))

(define optimization-type-files
  (list (cons "orig" all-files)
        (cons "coached" all-files)
        (cons "hand-opt" hand-optimized-files)))

(define (mk-optimization-group opt-type files)
  (bgroup
   opt-type
   (map
    (lambda (f)
      (let ([f-path (format "oc-external/~a/~a" opt-type f)])
        (mk-shell-benchmark
         f
         (format "racket ~a" f-path)
         #:configure (format "raco make ~a" f-path))))
    files)))

(define benchmarks
  (bgroup
   ""
   (map
    (lambda (otfs)
      (mk-optimization-group (car otfs) (cdr otfs)))
    optimization-type-files)))

(define results
  (run-benchmarks benchmarks (bopts #:num-trials 5 #:discard-first #f)))

(parameterize ([plot-x-ticks no-ticks]
               [plot-x-tick-label-anchor 'top-right]
               [plot-x-tick-label-angle 30])
  (plot-file
   #:title "optimization coach"
   #:x-label #f
   #:y-label "normalized time"
   (render-benchmark-alts (map car optimization-type-files) results "orig")
   "optimization-coach.pdf"))
