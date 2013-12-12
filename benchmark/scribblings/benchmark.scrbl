#lang scribble/manual

@(require (for-label racket plot plot/utils racket/runtime-path racket/set "../main.rkt"))
@(require scribble/eval scribble/core)

@title[#:tag "top"]{Benchmark}
@author{@(author+email "Josh McGrath" "mcgrathj@ccs.neu.edu")}

@defmodule[benchmark]

The goal of the @(racket benchmark) library is to reduce the effort of
writing benchmark harnesses. Specifically, this
library aims to handle the sampling and plot rendering of results using
sound statistical methods. This library is designed for comparing:
different versions of Racket for a fixed set of programs, different versions
of programs with the same version of Racket, or both (different versions
of Racket and different versions of the programs). But as the examples in
the first two sections will show, it is not limited to these use cases.

@table-of-contents[]

@section[#:tag "simple example"]{Example: Measuring the Impact of Racket's JIT}

In the following example we evaluate the use of Racket's JIT on two
simple programs, running each with and without JIT, and then plot
the results. The plot renderer groups together the results of running the
same program under different options, distinguishing the different
options using colors.

@#reader scribble/comment-reader
(interaction
  (require benchmark plot racket racket/runtime-path)

  (define-runtime-path fib-path
    "examples/macro-examples/fib.rkt")
  (define-runtime-path collatz-path
    "examples/macro-examples/collatz1000.rkt")
  (define-runtime-path compiled-dir
    "examples/macro-examples/compiled")

  (define results
    (run-benchmarks
     ;; files to run (whats)
     (list fib-path collatz-path)
     ;; list of options (hows)
     (list (list 'jit 'no-jit))
     ;; how to run each benchmark
     (lambda (file jit)
       (if (equal? jit 'jit)
           (system* (find-executable-path "racket") file)
           (system* (find-executable-path "racket") "-j" file)))
     #:build
     (lambda (file jit)
       (system* (find-executable-path "raco") "make" file))
     #:clean
     (lambda (file jit)
       (system* (find-executable-path "rm") "-r" "-f" compiled-dir))
     #:num-trials 30
     #:make-name (lambda (path)
                   (let-values
                       ([(_1 file-name _2) (split-path path)])
                     (path->string file-name)))))

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

@racketmod[#:file "fib.rkt"
  racket
  (define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))

  (time (fib 26))
]

@racketmod[#:file "collatz1000.rkt"
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

Here we use @(racket 'delta-time) to demonstrate measuring the time
to evaluate @(racket (run ...)). At a high level we will be evaluating
lists vs vectors for map and append. Specifically, map and append
(or the appropriate variant) will correspond to our whats, and the
list/vector size and whether we are doing list or vector operations
will corespond to our hows. That is, for each input size, for each
of list and vector, we will evaluate map and append.

@#reader scribble/comment-reader
(interaction
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

@section[#:tag "running macro benchmarks"]{Running Benchmarks}
@defproc[(run-benchmarks
          [whats (listof string?)]
          [hows (listof (listof any/c))]
          [run procedure?]
          [#:build build (or/c procedure? #f) #f]
          [#:clean clean (or/c procedure? #f) #f]
          [#:extract-time extract-time (or/c 'delta-time (-> string benchmark-trial-time?))
                          racket-time-extract-result]
          [#:num-trials num-trials exact-integer? 30]
          [#:make-name make-name (-> string? string?) identity]
          [#:skip skip procedure? (lambda _ #f)])
         (listof benchmark-result?)]{

For each @(racket what), @(racket run) is executed for each
combination of one element from each of the elements of @(racket hows).
Thus, the arity of @(racket run) must be @(racket (+ 1 (length hows))).

@(racket build) and @(racket clean) are functions with the same argument
types as @(racket run) and are run before and after each @(racket num-trials)
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
it evaluates to true the associated benchmark/options combination
is skipped.

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

@elem[#:style (style #f (list (color-property "red")))]{
As has been pointed out by Neil Toronto, the method
of normalization used by @(racket render-benchmark-alts) produces
confidence intervals that are too narrow. This needs to be fixed...
}

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

@defparam[current-benchmark-color-scheme benchmark-color-scheme (cons/c (listof plot-color/c) (listof plot-brush-style/c))]{
Default is @(racket pastel-color-scheme). Parameter controlling
color scheme used by @(racket render-benchmark-alts).
}

Available color schemes:
@deftogether[(@defthing[bright-color-scheme (cons/c (listof plot-color/c) (listof plot-brush-style/c))]
@defthing[pastel-color-scheme (cons/c (listof plot-color/c) (listof plot-brush-style/c))]
@defthing[black-white-color-scheme-short (cons/c (listof plot-color/c) (listof plot-brush-style/c))]
@defthing[black-white-color-scheme-medium-1 (cons/c (listof plot-color/c) (listof plot-brush-style/c))]
@defthing[black-white-color-scheme-medium-2 (cons/c (listof plot-color/c) (listof plot-brush-style/c))]
@defthing[black-white-color-scheme-long (cons/c (listof plot-color/c) (listof plot-brush-style/c))])]{
A few color schemes provided for convenience. Length (short, medium, and long)
corresponds to the number of distinct colors that are used. 'short' is 3,
'medium' is 5, and 'long' is 8. In the event that there are more
unique combinations of options, the colors wrap around.
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
