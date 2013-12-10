#lang scribble/manual

@(require (for-label racket plot racket/set "../main.rkt"))
@(require scribble/eval scribble/core)

@title[#:tag "top"]{Benchmark}
@author{@(author+email "Josh McGrath" "mcgrathj@ccs.neu.edu")}

@defmodule[benchmark]

@elem[#:style (style #f (list (color-property "red")))]{
As has been pointed out by Neil Toronto, the method
of normalization used by @(racket render-benchmark-alts) produces
confidence intervals that are too narrow. This needs to be fixed...
}

The goal of the @(racket benchmark) library is to reduce the effort of
writing benchmark harnesses for macro benchmarks. Specifically, this
library aims to handle the sampling and comparing of results using
sound statistical methods. A typical use for this library is comparing
different versions of Racket for a fixed set of programs.

At a high level, benchmarks are represented as a Cartesian product
of (n+1) vectors, where one vector represents what (e.g. the files) to
be run, and the remaining n vectors represent how (e.g. flags or
binaries) to run. These vectors are supplemented by a user-specified
function dictating how to run.

@table-of-contents[]

@section[#:tag "simple example"]{Example: Measuring Racket's JIT}

In the following example we evaluate the use of Racket's JIT on two
simple programs, running each with and without JIT, and then plot
the results. The plot renderer groups together the results of running the
same program under different options, distinguishing the different
options using colors.

@#reader scribble/comment-reader
(interaction
  (require benchmark plot racket/system)

  (define results
    (run-benchmarks
     ;; files to run (whats)
     (list "fib.rkt" "collatz1000.rkt")
     ;; list of options (hows)
     (list (list 'jit 'no-jit))
     ;; how to run each benchmark
     (lambda (file jit)
       (system (format "racket ~a examples/macro-examples/~a"
                       (if (equal? jit 'jit) "" "-j")
                       file)))
     #:build
     (lambda (file jit)
       (system (format "raco make examples/macro-examples/~a" file)))
     #:clean
     (lambda (file jit)
       (system "rm -rf examples/macro-examples/compiled"))
     #:num-trials 30))

  (parameterize ([plot-x-ticks no-ticks])
    (plot-pict
     #:title "jit vs no jit"
     #:x-label #f
     #:y-label "normalized time"
     (render-benchmark-alts
      ;; default options
      (list 'jit)
      results)))
  )

@racketmod[#:file "../examples/macro-examples/fib.rkt"
  racket
  (define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))

  (time (fib 26))
]

@racketmod[#:file "../examples/macro-examples/collatz1000.rkt"
  racket
  (define (collatz n)
    (if (even? n)
        (/ n 2)
        (+ (* 3 n) 1)))

  (define (collatz-range m)
    (for-each (lambda (n) (collatz n))
              (stream->list (in-range 0 m))))

  (time (collatz-range 1000))
]


It's important to note that both examples explicitly call time.
This is because, by default, the time of a benchmark is not the time to
evaluate @(racket (run ...)), but rather the time reported
to stdout by @(racket (run ...)). Had we merely wanted to time how long
it took to evaluate @(racket (run ...)) we would have called
@(racket run-benchmarks) with @(racket #:extract-time 'delta-time).
Such an example will be given in the next section.

@section[#:tag "simple example fibonacci and jit"]{Example: Vectors vs Lists}

@elem[#:style (style #f (list (color-property "red")))]{
This section needs a lot of work
}

Here we use @(racket 'delta-time) to demonstrate measuring the time
to evaluate @(racket (run ...)). At a high level we will be evaluating
lists vs vectors for map and append. Specifically, map and append
(or the appropriate variant) will correspond to our whats, and the
list/vector size and whether we are doing list or vector operations
will corespond to our hows. That is, for each input size, for each
of list and vector, we will evaluate map and append.

@#reader scribble/comment-reader
(interaction
  (require benchmark plot racket/match racket/vector racket/stream)

  ;; list/vector sizes
  (define sizes (list 50000 100000))

  (define lists (map (lambda (i) (build-list i values)) sizes))

  (define vectors (map list->vector lists))

  (define results
    (run-benchmarks
     ;; operations (whats)
     (list 'square-map 'self-append)
     ;; list of options (hows)
     (list
      ;; sizes (and their indices) in the sizes list
      (map cons (build-list (length sizes) values) sizes)
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
  )

@section[#:tag "running macro benchmarks"]{Running Macro Benchmarks}
@defproc[(run-benchmarks
          [whats (listof string?)]
          [hows (listof (listof any/c))]
          [run procedure?]
          [#:build build (or/c procedure? #f) #f]
          [#:clean clean (or/c procedure? #f) #f]
          [#:extract-time extract-time (or/c 'delta-time (-> string benchmark-trial-time?))
                          racket-time-extract-result]
          [#:num-trials num-trials exact-integer? 30]
          [#:make-name make-name (-> string? string?) (lambda (x) x)]
          [#:skip skip procedure? (lambda (r . rest) #f)])
         (listof benchmark-result?)]{

@(racket run) is executed on each combination of one
item from @(racket whats) and one element from each of the elements
of @(racket hows). Thus, the arity of @(racket run) must be
@(racket (+ 1 (length hows))).

@(racket build) and @(racket clean) are functions with the same argument
types as @(racket run) and are run before each @(racket num-trials)
runs of @(racket run).

When not @(racket 'delta-time), @(racket extract-time) is
applied to the contents of stdout after each run of @(racket run),
and must produce a @(racket benchmark-trial-time?).
In this case it is expected that each of the @(racket whats) report
their time via stdout when run. When @(racket 'delta-time),
the time of the benchmark is the time to evaluate @(racket (run args ...)).

@(racket make-name) takes an element of @(racket whats) and
is expected to produce the name for the result. This is useful
for stripping directories or file extensions.

@(racket skip) has the same argument types as @(racket run), and when
it evaluates to true the associated benchmark is skipped.

Benchmarks are logically specified as an (n+1)-dimensional
matrix where n of the dimensions are options (how to run) and one dimension is
the names/files (what to run). The names/files dimension is specified in the
@(racket whats) list. The options dimensions are specified as
lists in the @(racket hows) list.

In @secref["simple example"], @(racket (list "fib.rkt" "collatz1000.rkt")) is
the @(racket whats) and @(racket (list (list 'jit 'no-jit))) is the
@(racket hows). That is, there is only one dimension of options for
running @(racket (list 'jit 'no-jit)), and two things to run
@(racket "fib.rkt") and @(racket "collatz1000.rkt").

So, execution might look something like:
@verbatim{
building (fib.rkt jit)
running (fib.rkt jit) ... (30 times)
cleaning (fib.rkt jit)
building (fib.rkt no-jit)
running (fib.rkt no-jit) ... (30 times)
cleaning (fib.rkt no-jit)
building (collatz1000.rkt jit)
running (collatz1000.rkt jit) ... (30 times)
cleaning (collatz1000.rkt jit)
building (collatz1000.rkt no-jit)
running (collatz1000.rkt no-jit) ... (30 times)
cleaning (collatz1000.rkt no-jit)
}

}

@subsection[#:tag "extracting time"]{Extracting Reported Time}

@defproc[(racket-time-extract-result [str string?])
         benchmark-trial-time?]{
To be used with @(racket #:extract-time) for the output of
Racket's @(racket time). Note: this is the default for the
@(racket #:extract-time) argument of @(racket run-benchmarks).
}

@defstruct[benchmark-trial-time? ([cpu real?]
                                  [real real?]
                                  [gc real?])
           #:prefab]{
Data structure for reporting the time of a single trial. This is
exposed to allow the user to write their own @(racket #:extract-time)
function.
}

@section[#:tag "Plotting"]{Plotting}
@(racket benchmark) exports a @(racket renderer2d?) for plotting results
of benchmarks using the @(racket plot) library.

@defproc[(render-benchmark-alts
          [norm-opts (listof any/c)]
          [benchmark-results (listof benchmark-result?)]
          [#:format-opts format-opts
                         (-> (listof any/c) string?)
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
@(racket run-benchmarks).

@(racket format-opts) formats the list of options into strings
for displaying on the legend of the plot.
}

@subsection[#:tag "Color Schemes"]{Color Schemes}
Color schemes are used to make benchmark plots more readable by
coloring benchmarks according to the options (hows). In @secref["simple example"]
the 'jit' option is salmon and the 'no-jit' option blue.

@defparam[current-benchmark-color-scheme benchmark-color-scheme (cons/c (listof plot-color/c) plot-brush-style/c)]{
Default is @(racket pastel-color-scheme). Parameter controlling
color scheme used by @(racket render-benchmark-alts).
}

Available color schemes:
@deftogether[(@defthing[bright-color-scheme (cons/c (listof plot-color/c) plot-brush-style/c)]
@defthing[pastel-color-scheme (cons/c (listof plot-color/c) plot-brush-style/c)]
@defthing[black-white-color-scheme-short (cons/c (listof plot-color/c) plot-brush-style/c)]
@defthing[black-white-color-scheme-medium-1 (cons/c (listof plot-color/c) plot-brush-style/c)]
@defthing[black-white-color-scheme-medium-2 (cons/c (listof plot-color/c) plot-brush-style/c)]
@defthing[black-white-color-scheme-long (cons/c (listof plot-color/c) plot-brush-style/c)])]{
A few color schemes provided for convenience. Length (short, medium, and long)
corresponds to the number of distinct colors that are used. 'short' is about 3,
'medium' is about 5, and 'long' is about 8. In the event that there are more
unique products of options, the colors wrap around.
}

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
