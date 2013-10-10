#lang scribble/manual

@(require (for-label racket) scribble/eval)


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

@examples[
(require benchmark plot racket/set)

(define list-sizes (list 10 100 1000))

(define sample-lists
  (for/list ([i list-sizes])
    (for/list ([j i]) (* 2 j))))

(define sample-sets
  (for/list ([l sample-lists]) (list->set l)))

(define moduli (list 12 17 39))
(define (divides m n) (= 0 (modulo n m)))
(define (interesting m)
  (ormap (lambda (n) (divides n m)) moduli))
(define sample-vals
  (map (lambda (l) (filter interesting l)) sample-lists))

(define (mk-bench lookup-fn set vals set-count)
  (b1
   (format "lookup ~a of ~a vals" (length vals) set-count)
   (map (lambda (v) (lookup-fn set v)) vals)))

(define benches
  (bgroup
   ""
   (list
    (mk-bgroup
     "list set"
     (map (lambda (set vals)
            (mk-bench (lambda (lst v) (member v lst)) set vals (length set)))
          sample-lists
          sample-vals))
    (mk-bgroup
     "racket set"
     (map (lambda (set vals)
            (mk-bench set-member? set vals (set-count set)))
          sample-sets
          sample-vals)))))

(define results
  (run-benchmarks
   benches
   (bopts #:gc-between #f
          #:itrs-per-trial 100
          #:num-trials 50)))


(parameterize ([plot-x-ticks no-ticks])
  (plot-pict
   #:title "sets"
   #:y-label "normalized time"
   #:x-label #f
   (render-benchmark-alts
    (list "racket set" "list set")
    results
    "racket set")))]
