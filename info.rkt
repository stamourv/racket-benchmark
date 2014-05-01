#lang setup/infotab

(define collection 'multi)
(define deps '("base" "math-lib" "plot-gui-lib" "plot-lib" "srfi-lite-lib"
               "typed-racket-lib"))
(define build-deps '("plot-doc" "racket-doc" "rackunit-lib" "scribble-lib"))

(define pkg-desc "Benchmarking library.")

(define version "0.4")

(define pkg-authors '(mcgrathj stamourv))
