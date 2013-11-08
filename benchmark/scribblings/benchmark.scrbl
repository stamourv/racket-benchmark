#lang scribble/manual

@(require (for-label racket plot racket/set "../main.rkt") scribble/eval)

@title[#:tag "top"]{Benchmark}
@author{@(author+email "Josh McGrath" "mcgrathj@ccs.neu.edu")}

@defmodule[benchmark]

The goal of the @(racket benchmark) library is to reduce the effort of
writing benchmark harnesses. In the simplest case, only the
expressions to be evaluated must be specified. At the same time,
control over gc, number of iterations, what precisely is timed,
etc. is exposed. Additionally, a @(racket renderer2d?) for use with
the @(racket plot) library is provided.

@table-of-contents[]

@section[#:tag "simple example"]{Example 1: Benchmarking Sleep}
To gain some confidence in our benchmarking framework, let's begin by
benchmarking Racket's @(racket sleep).

@#reader scribble/comment-reader
(examples
 (require plot benchmark)

 ;; set parameters for benchmarks
 (gc-between #f)
 (itrs-per-trial 1)
 (num-trials 30)

 ;; benches : (listof benchmark-one?)
 (define benches
   (list
    (bench-one "sleep .1" (sleep .1))
    (bench-one "sleep .2" (sleep .2))
    (bench-one "sleep .3" (sleep .3))))

 ;; results : (listof benchmark-result?)
 (define results (run-benchmarks benches))

 (parameterize ([plot-x-ticks no-ticks])
   (plot-pict #:x-label #f #:y-label "normalized time"
              (render-benchmark-alts
               (list "sleep .1" "sleep .2" "sleep .3") "sleep .1" results)))
 )

This example illustrates a few features of this library.

The first is the parameters @(racket gc-between), @(racket
itrs-per-trial), and @(racket num-trials).
@(racket (gc-between #f)) specifies not to run gc between
trials. @(racket (itrs-per-trial 1)) specifies to time how
long it takes to evaluate each expression once. Lastly,
@(racket num-trials 30) specifies to time each benchmark @(racket 30)
times.

The second feature is @(racket bench-one). We specify a @(racket
string?) @(racket "sleep .1") for the name of the benchmark and an
expression @(racket (sleep .1)) for the expression to benchmark.

Third, we run the list of @(racket benchmark-one?) that we created,
using @(racket run-benchmarks). @(racket benchmark-one?) is the
underlying structure used for representing an individual benchmark.

Last, we use @(racket render-benchmark-alts) to take the results that
we got from @(racket run-benchmraks) and create a renderer to use with
@(racket plot).

@section[#:tag "racket version example"]{Example 2: Running Different Versions of Racket}

It should also be easy to specify benchmarks that compare different
versions of Racket. Suppose we have two Racket files @(racket "a.rkt")
and @(racket "b.rkt") that report their times to execute via stdout
using @(racket time). We wish to test our Racket implementations against
these files. The following code will construct benchmarks to run each
file @(racket 30) times, without performing gc.

Now we can use @(racket run-benchmarks) to get our results.

@(racketblock
  (num-trials 30)
  (gc-between #f)

  (define benches
    (list
     (make-shell-bench "racket a.rkt" "a.rkt" (list))
     (make-shell-bench "racket b.rkt" "b.rkt" (list)))))

@section[#:tag "single benchmark"]{Individual Benchmarks}

@subsection[#:tag "benchmarking expresions"]{Benchmarking Expressions}

@defproc[(make-bench-one [name string?]
                         [thunk procedure?]
                         [#:gc-between gc-between boolean? (gc-between)]
                         [#:num-trials num-trials exact-integer? (num-trials)]
                         [#:itrs-per-trial itrs-per-trial exact-integer? (itrs-per-trial)]
                         [#:discard-first discard-first boolean? (discard-first)]
                         [#:manual-report-time manual-report-time boolean? (manual-report-time)])
         benchmark-one?]{
Produces a @(racket benchmark-one?) with associated @(racket name) for
computing the time to evaluate @(racket thunk) with specified
options. Options can also be specified by setting the appropriate
parameters directly. @(racket benchmark-one?) is the internal
structure used to represent something that can be run as a benchmark.
}

@#reader scribble/comment-reader

(examples
 (require benchmark)

 (make-bench-one "sleep .1" (lambda () (sleep .1)))
 ;; gc takes a long time to run, so we need to only run it once
 ;; to get a reasonably-sized (~ 50 ms) time
 ;; also don't run gc between each trial, because that's what we
 ;; are trying to measure
 (make-bench-one "gc" (lambda () (collect-garbage))
                 #:gc-between #f
                 #:itrs-per-trial 1))

@defform*[((bench-one name expr)
           (bench-one expr))]{
The @(racket bench-one) macro is merely a wrapper for @(racket
make-bench-one) to admit raw expressions where thunks are required in
@(racket make-bench-one) and to derive a name from the expression if no
name is given. Because keyword arguments are not currenty accepted by
@(racket bench-one), @(racket parameterize) should be used to change
options for running the benchmark.
}

Using the examples from @(racket make-bench-one):

@#reader scribble/comment-reader

(examples
 (require benchmark racket/function)

 (bench-one (sleep .1))

 (parameterize ([gc-between #f]
                [itrs-per-trial 1])
    (bench-one "gc" (collect-garbage))))

@subsection[#:tag "benchmarking shell commands"]{Benchmarking Shell Commands}

@defproc[(make-shell-bench
          [name string?]
          [command (or/c string-no-nuls? bytes-no-nuls?)]
          [#:configure configure (or/c #f procedure? string-no-nuls? bytes-no-nuls?) #f]
          [#:build build (or/c #f procedure? string-no-nuls? bytes-no-nuls?) #f]
          [#:extract-result extract-result (-> bytes? benchmark-trial-time?) racket-time-extract-result
           ]
          [#:clean clean (or/c #f procedure? string-no-nuls? bytes-no-nuls?) #f]
          [#:num-trials num-trials exact-integer? (num-trials)]
          [#:discard-first discard-first boolean? (discard-first)])
         benchmark-one?]{

Produces a @(racket benchmark-one?) with associated @(racket name) for
evaluating the time reported to evaluate @(racket (system command)).

@(racket (or/c string-no-nuls? bytes-no-nuls?)) is the contract on
commands that can be run with @(racket system).

Time must be reported by @(racket command) via stdout so that
that @(racket extract-result) can produce a @(racket
benchmark-trial-time?). Note that the default @(racket extract-time)
parses the output format of Racket's @(racket time).

}

A common use of shell benchmarks, however, is for comparing different
versions of racket. Consequently, a wrapper on top of
@(racket make-shell-bench) is provided.

@defproc[(make-racket-file-bench
          [name string?]
          [fname string?]
          [args (listof string?)]
          [#:configure configure (or/c #f procedure? string-no-nuls? bytes-no-nuls?) #f]
          [#:build build (or/c #f procedure? string-no-nuls? bytes-no-nuls?) (format "raco make ~a" fname)]
          [#:extract-result extract-result (-> bytes? benchmark-trial-time?) racket-time-extract-result
           ]
          [#:clean clean (or/c #f procedure? string-no-nuls? bytes-no-nuls?) #f]
          [#:num-trials num-trials exact-integer? (num-trials)]
          [#:discard-first discard-first boolean? (discard-first)])
         benchmark-one?]{

Produces a @(racket benchmark-one?) for benchmarking the time
reported by running racket on file name @(racket fname) with
arguments @(racket args). The time is expected to be in the format
of @(racket time). Also note that @(racket raco make)
builds @(racket fname) prior to running.

}

@subsubsection[#:tag "extracting time"]{Extracting Reported Time}

@defproc[(racket-time-extract-result [str bytes?])
         benchmark-trial-time?]{
To be used with @(racket #:extract-result) for the output of
Racket's @(racket time). Note: this is the default for the
@(racket #:extract-result) argument of @(racket make-shell-benchmark).
}

@defproc[(linux-time-extract-result [str bytes?])
         benchmark-trial-time?]{
To be used with @(racket #:extract-result) for the output of @(racket
/usr/bin/time -p) (i.e. POSIX standard 1003.2). Please note that
/usr/bin/time reports time to stderr, so it must
be redirected to stdout, as only stdout is captured.

Parses @(racket
/usr/bin/time -p) output and sets @(racket cpu) and @(racket real)
fields of @(racket benchmark-trial-time?) to "user" time reported by
@(racket /usr/bin/time -p). The @(racket gc) of @(racket
benchmark-trial-time?) is set to @(racket 0).
}

Suppose we have a file @(racket "a.rkt") in our current directory
that has the following contents:

@#reader scribble/comment-reader

@codeblock|{
  #lang racket
  (time (sleep (random)))
}|

@#reader scribble/comment-reader

(examples
 (require benchmark)

 ;; run 'racket a.rkt' using default for #:extract-result
 (make-shell-bench
  "a.rkt"        ;; name of benchmark
  "racket a.rkt" ;; command to run using system
  )

 (make-racket-file-bench "a.rkt" "a.rkt" (list)))

@defstruct[benchmark-trial-time? ([cpu real?]
                                  [real real?]
                                  [gc real?])
           #:prefab]{
Data structure for reporting the time of a single trial. This is
exposed to allow the user to write their own @(racket #:extract-time).
}

@section[#:tag "grouping benchmarks"]{Groups of Benchmarks}
@defproc[(make-bench-group [name string?]
                           [benchmarks (listof (or/c benchmark-one? benchmark-group?))])
         benchmark-group?]{}

Creates a @(racket benchmark-group?) with specified @(racket name)
for specified @(racket benchmarks). Groups merely name
lists of @(racket (or/c benchmark-one? benchmark-group?)).

@#reader scribble/comment-reader
(examples
 (require benchmark)

 (define (fib n)
   (if (> 2 n) n (+ (fib (- n 1)) (fib (- n 2)))))

 (define benches
   (make-bench-group
    "fibs" ;; group name
    (list
     (make-bench-group
      "odds"
      (list
       (bench-one "25" (fib 25))
       (bench-one "27" (fib 27))))
     (make-bench-group
      "evens"
      (list
       (bench-one "26" (fib 26))
       (bench-one "28" (fib 28))))))))

The name associated with the results of each benchmark run is formed
by taking the '/'-separated path of names from the root of the
benchmark tree, where @(racket benchmark-group?) represent the
internal nodes and @(racket benchmark-one?) the leaves. For example,
@(racket "fibs/evens/28") and @(racket "fibs/odds/25").

@section[#:tag "benchmark options"]{Options for Running Benchmarks}
This section explains parameters and keyword arguments to
change the following behaviors of running benchmarks: gc,
number of trials, number of iterations per trial, whether to
discard the first result, and manually reporting time.

@subsection[#:tag "gc-between"]{GC Between}
@defboolparam[gc-between new-gc-between]{
Default @(racket #t). The parameter @(racket gc-between) specifies
whether gc should be run before each trial of a benchmark. If @(racket
#t) (the default), the GC is invoked thrice before each trial, using
@(racket (collect-garbage)). Something about finalizers...
}

@subsection[#:tag "num-trials"]{Number of Trials}
@defparam[num-trials new-num-trials exact-integer?]{
Default @(racket 100). The parameter @(racket num-trials) specifies
the number of trials (samples) to gather for evaluating the thunk
being benchmarked.
}

@subsection[#:tag "itrs-per-trial"]{Iterations per Trial}
@defparam[itrs-per-trial new-itrs-per-trial exact-integer?]{
Default @(racket 10000). The parameter @(racket itrs-per-trial)
specifies the number of times to evaluate the thunk during each
trial. That is, the time reportied by a trial is the time to evaluate
the thunk @(racket itrs-per-trial) times. This is meant to guarantee
that the thunk is evaluated enough times so that clock tick resolution
isn't significant. More text about this... Should probably abstract
this away from the user.
}

@subsection[#:tag "discard-first"]{Discard First}
@defboolparam[discard-first new-discard-first]{
Default @(racket #t).  The parameter @(racket discard-first)
determines whether the first trial time is discarded. The purpose for
this is to discard overhead due to initialization.
}

@subsection[#:tag "manual-report-time"]{Manually Reporting Time}
@defboolparam[manual-report-time new-manual-report-time]{
Default @(racket #f). When @(racket manual-report-time) is @(racket
#f), timing is performed using @(racket time-apply) and not the
responsibility of the consumer of this library. When @(racket
manual-report-time) is set to @(racket #t), it is the responsibility
of the user to report the time of each trial using @(racket
time-internal).  This is the mechanism used for reporting times of
shell benchmarks, but it can also be used for expression benchmarks.
}

@section[#:tag "running benchmarks"]{Runing Benchmarks}
@defproc[(run-benchmarks [benchmarks (listof (or/c benchmark-one? benchmark-group?))]
                         [opts (or/c benchmark-opts? #f) #f])
         (listof benchmark-result?)]{
Given a collection of benchmarks, runs each benchmark according
to its options, and produces a list of @(racket benchmark-result?).
}

@section[#:tag "Benchmark Times"]{Benchmark Times}
@defproc[(time-internal [thunk procedure?])
         void?]{
    When uesd with @(racket #:manual-report-time #f) in @(racket benchmark-opts?),
    reports the time required to evaluate @(racket (thunk)) for a single trial.
}

@section[#:tag "Plotting"]{Plotting}
@(racket benchmark) exports a @(racket renderer2d?) for plotting results
of benchmarks using the @(racket plot) library.

@defproc[(render-benchmark-alts [alt-names (listof string?)]
                                [norm-alt-name string?]
                                [benchmark-results (listof benchmark-result?)])
         renderer2d?]{
Produces a @(racket renderer2d?) of speedupfor the specified benchmark
results with error bars based on 95% confidence intervals.

This function has two modes: comparing groups of benchmarks to
other groups of benchmarks, and comparing individual benchmarks
to other individual benchmarks.

In the former case, we specify the names of the groups to compare as
the @(racket alt-names) and the standard group to use as @(racket
norm-alt-name). Then for each benchmark in the standard group, the
speedup of the corresponding benchmark in each alternative group is
rendered.

A good example of this can be seen in 'examples/fusion.rkt' referenced
in the @(secref "additional examples") section.

In the case of plotting speedup of individual benchmarks, the @(racket
norm-alt-name) (standard) benchmark is the name of an individual
benchmark. In this case, the speedup of each of the @(racket
alt-names) is rendered.

A good example of this can be seen in 'examples/fib.rkt' referenced
in the @(secref "additional examples") section.
}

@subsection[#:tag "Color Schemes"]{Color Schemes}
Color schemes are used to make benchmark plots more readable by
coloring corresponding benchmarks in different groups the same.

@defparam[current-benchmark-color-scheme benchmark-color-scheme (cons/c (listof plot-color/c) plot-brush-style/c)]{
Default is @(racket pastel-color-scheme). Parameter controlling
color scheme used by @(racket render-bencharmk-alts).
}

Available color schemes:
Confused by defidentifier. Keep getting 'unbound identifier' errors
1. bright-color-scheme
2. pastel-color-scheme
3. black-white-color-scheme-short
4. black-white-color-scheme-medium-1
5. black-white-color-scheme-medium-2
6. black-white-color-scheme-long

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

@section[#:tag "miscellaneous"]{Miscellaneous}
@defparam[min-samples num exact-integer?]{
Controls the minium number of samples needed to calculate confidence
intervals. By default this is set to @(racket 30), but it is often
useful to reduce this while debugging so that statistics can be
calculated for fewer than 30 samples.

An error message of the form "number of samples (~a) must be >= ~a"
indicates that @(racket num-trials) for some benchmark is less than
@(racket (min-samples)).
}

@section[#:tag "additional examples"]{Additional Examples}
Full examples can be found in the 'benchmark/examples/' directory
of the repository: 'github.com/joshmcgrath08/racket-benchmark'.

1. color-schemes.rkt shows the use of color schemes.

2. fib.rkt (ab)uses the benchmark framework to show the growth
   of the naive fibonacci function. Here we take "fib 18" to be
   our standard, and plot the alternatives "fib 18", "fib 19",
   "fib 20", "fib 21", and "fib 22".

3. fusion.rkt compares repeated map operations with the map of
   a composed function. E.g. @(racket (map f (map g lst))) with
   @(racket (map (lambda (x) (f (g x))) lst)). Here, groups are
   used to distinguish those expressions with composed functions
   ("fusion"), and those without ("no-fusion"). The use of
   @(racket render-benchmrak-alts) shows that we are considering
   the groups "fusion" and "no-fusion" to be our alternatives, with
   "fusion" constituting our standard. That is, for each benchmark
   in the "no-fusion" group, we will standardize its time against
   the corresponding benchmark in the "fusion" group and plot the
   results.

@;; Local Variables:
@;; compile-command: "raco setup benchmark"
@;; End:
