#lang scribble/manual

@(require (for-label racket plot racket/set "../main.rkt") scribble/eval)

@title[#:tag "top"]{Benchmark}
@author{@(author+email "Josh McGrath" "mcgrathj@ccs.neu.edu")}

@defmodule[benchmark]

The goal of the @(racket benchmark) library is to reduce the effort of
writing benchmark harnesses for macro benchmarks.
Additionally, a @(racket renderer2d?) for use with
the @(racket plot) library is provided.

@table-of-contents[]

@section[#:tag "simple example"]{Example: Measuring Racket's JIT}

@subsection[#:tag "macro benchmark example 1"]{The Harness}
@interaction[
  (require benchmark plot racket)

  (define (jit-flag s)
    (if (equal? s 'jit)
        ""
        "-j"))

  (define results
    (run-macro-benchmarks
     (list "fib.rkt" "collatz1000.rkt")
     (list (list 'jit 'no-jit))
     (lambda (file jit)
       (system
        (format "racket ~a examples/macro-examples/~a" (jit-flag jit) file)))
     #:build
     (lambda (file jit)
       (system (format "raco make examples/macro-examples/~a" file)))
     #:clean
     (lambda (file jit)
       (system "rm -rf examples/macro-examples/compiled"))))

  (parameterize ([plot-x-ticks no-ticks])
    (plot-pict
     #:title "jit vs no jit"
     #:x-label #f
     #:y-label "normalized time"
     (render-benchmark-alts (list 'jit) results)))
  ]

@subsection[#:tag "macro examples fib"]{File 1: examples/macro-examples/fib.rkt}
@racketblock[
  (define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))
  (time (fib 26))
  ]

@subsection[#:tag "macro examples collatz"]{File 2: examples/macro-examples/collatz1000.rkt}
@racketblock[
   (define (collatz n)
    (if (even? n)
        (/ n 2)
        (+ (* 3 n) 1)))
  (define (collatz-range m)
    (for-each (lambda (n) (collatz n))
              (stream->list (in-range 0 m))))
  (time (collatz-range 1000))
  ]

@section[#:tag "running macro benchmarks"]{Running Macro Benchmarks}
@defproc[(run-macro-benchmarks
          [whats (listof string?)]
          [hows (listof (listof any/c))]
          [run (procedure?)]
          [#:build build (or/c procedure? #f) #f]
          [#:clean clean (or/c procedure? #f) #f]
          [#:extract-time extract-time (-> bytes benchmark-trial-time?)
                          racket-time-extract-result]
          [#:num-trials num-trials exact-integer? 30]
          [#:make-name make-name (-> string? string?) id])
         (listof benchmark-result?)]{

Macro benchmarks are logically specified as an (n+1)-dimensional
matrix where n of the dimensions are options (how to run) and one dimension is
the names/files (what to run). The names/files dimension is specified in the
@(racket whats) list. The options dimensions are specified as
lists in the @(racket hows) list.

In the example above, @(racket (list "fib.rkt" "collatz1000.rkt")) is
the @(racket whats) and @(racket (list (list 'jit 'no-jit))) is the
@(racket hows). That is, there is only one dimension of options for
running @(racket (list 'jit 'no-jit)), and two things to run
@(racket "fib.rkt") and @(racket "collatz1000.rkt").

@(racket run) is executed on each combination of one
item from @(racket whats) and one element from each of the elements
of @(racket hows). So in the example above, @(racket run) has an
arity of 2 because it must accept an element of the @(racket whats)
and one element from each element of the @(racket hows), and there
is only one element in @(racket hows) .

@(racket build) and @(racket clean) are functions with the same
type as @(racket run) and are run before each @(racket num-trials)
runs of @(racket run).

So for the example above, execution might look something like:
@verbatim{
building (fib.rkt jit)
running (fib.rkt jit)
running (fib.rkt jit) ...
cleaning (fib.rkt jit)
building (fib.rkt no-jit)
running (fib.rkt no-jit)
running (fib.rkt no-jit) ...
cleaning (fib.rkt no-jit)
building (collatz1000.rkt jit)
running (collatz1000.rkt jit)
running (collatz1000.rkt jit) ...
cleaning (collatz1000.rkt jit)
building (collatz1000.rkt no-jit)
running (collatz1000.rkt no-jit)
running (collatz1000.rkt no-jit) ...
cleaning (collatz1000.rkt no-jit
}

@(racket extract-time) is applied to the contents of stdout after
each run of @(racket run), and must produce a @(racket benchmark-trial-time?).
Thus, it is expected that each of the @(racket whats) report
their time via stdout when run.

@(racket make-name) takes an element of @(racket whats) and
is expected to produce the name for the result. This is useful
for stripping directories or file extensions.

}

@subsection[#:tag "extracting time"]{Extracting Reported Time}

@defproc[(racket-time-extract-result [str bytes?])
         benchmark-trial-time?]{
To be used with @(racket #:extract-time) for the output of
Racket's @(racket time). Note: this is the default for the
@(racket #:extract-time) argument of @(racket run-macro-benchmarks).
}

@defstruct[benchmark-trial-time? ([cpu real?]
                                  [real real?]
                                  [gc real?])
           #:prefab]{
Data structure for reporting the time of a single trial. This is
exposed to allow the user to write their own @(racket #:extract-time).
}

@section[#:tag "Plotting"]{Plotting}
@(racket benchmark) exports a @(racket renderer2d?) for plotting results
of benchmarks using the @(racket plot) library.

@defproc[(render-benchmark-alts
          [norm-opts (listof any/c)]
          [benchmark-results (listof benchmark-result?)]
          [#:format-opts format-opts
                         (-> (listof any/c) string)
                         (lambda (opts) (apply ~s opts #:separator " "))])
         renderer2d?]{
Produces a @(racket renderer2d?) of normalized time
for the specified benchmark results with error bars
based on 95% confidence intervals. The time of a benchmark is
normalized against the benchmark with the same name but
@(racket norm-opts) for options.

@(racket norm-opts) is the list of options
(from @(racket hows)) of @(racket run) that are the standard
options.

For example, @(racket (list 'jit)) is the list of standard
options in @secref["simple example"].

@(racket benchmark-results) are the results produced by
@(racket run-macro-benchmarks).

@(racket format-opts) formats the list of options into strings
for displaying on the legend of the plot.
}

@subsection[#:tag "Color Schemes"]{Color Schemes}
Color schemes are used to make benchmark plots more readable by
coloring corresponding benchmarks in different groups the same.

@defparam[current-benchmark-color-scheme benchmark-color-scheme (cons/c (listof plot-color/c) plot-brush-style/c)]{
Default is @(racket pastel-color-scheme). Parameter controlling
color scheme used by @(racket render-benchmark-alts).
}

Confused by defidentifier. Keep getting 'unbound identifier' errors.

Available color schemes:
@itemlist[#:style 'ordered
 @item{bright-color-scheme}
 @item{pastel-color-scheme}
 @item{black-white-color-scheme-short}
 @item{black-white-color-scheme-medium-1}
 @item{black-white-color-scheme-medium-2}
 @item{black-white-color-scheme-long}]

@section[#:tag "Persisting Results"]{Persisting Results}
Results can be persisted with @(racket record-results) and retrieved
with @(racket get-past-results).

@defproc[(record-results [results (listof benchmark-result?)]
                         [file path?])
         void?]
Persists @(racket results) to file-<n> where <n> is the smallest natural
such that file-<n> doesn't exist.

@defproc[(get-past-results [file path?]
                           [version (or/c exact-integer? #f) #f])
         (listof benchmark-result?)]{
If @(racket version) is specified by user, reads results from
file-<version>. Otherwise, reads results from file-<n> where
n is the largest natural such that file-<n+1> does not exist.
}

@;; Local Variables:
@;; compile-command: "raco setup benchmark"
@;; End:
