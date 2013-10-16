#lang racket

(require racket/runtime-path)

(provide results-file optimization-type-files)

(define-runtime-path results-file "benches/optimization-coach.bench")

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
