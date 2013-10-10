#lang scribble/manual

@(require (for-label racket) (for-label benchmark))

@title{Benchmark}

@section[#:tag "overview"]{Overview}

The goal of the benchmark library is to reduce the effort of writing
benchmarks for comparing alternatives. In the simplest case, only names
of specific benchmarks, names of containing groups, and expressions
must be specified. At the same time, control over gc, number of times run,
what precisely is timed, are exposed in case additional configuration is
required. Currently this library is in a pre-alpha state.

@section[#:tag "simple example"]{A Simple Example}
Suppose we have implemented sets using a list as our underlying data structure.
An obvious question is how its performance compares to racket's implementation
@racket[set]. For now we decide to focus our effors on the membership function.

@racketblock[

(define (my-set-member? v s)
  (member v s))

(define list-sizes (list 10 100 1000 5000 10000))

(define sample-sets
  (for/list ([i list-sizes])
    (for/list ([j i]) j)))

(define moduli (list 12 13 17 23 39))
(define (divides m n) (== 0 (modulo n m)))
(define (interesting m)
  (or-map (lambda (n) (divides n m)) moduli))

(define sample-vals
  (map (lambda (l) (filter interesting l)) sample-sets))

(define benches
  )
]
