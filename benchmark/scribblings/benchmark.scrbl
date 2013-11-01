#lang scribble/manual

@(require (for-label racket plot racket/set) scribble/eval)

@title[#:tag "top"]{Benchmark}
@author{@(author+email "Josh McGrath" "mcgrathj@ccs.neu.edu")}

@defmodule[benchmark]

The goal of the benchmark library is to reduce the effort of writing
benchmarks for comparing alternatives. In the simplest case, only the
expressions to be evaluated must be specified. At the same time,
control over gc, number of iterations, what precisely is timed,
etc. are exposed in case additional configuration is
desired.

@table-of-contents[]

@section[#:tag "simple example"]{A Simple Example}
To gain some confidence in our benchmarking framework, let's begin by
benchmarking Racket's @(racket sleep).

@#reader scribble/comment-reader

(examples
 (require plot benchmark)
 
 ;; fmt : num? -> string?
 ;; format benchmark name
 (define (fmt n) (format "sleep ~a" n))

 ;; times : (listof (>=/c 0))
 ;; times to sleep for
 (define times (list .1 .2 .3))

 ;; benches : (listof benchmark-one?)
 (define benches
   (map (lambda (n) (bench-one (fmt n) (sleep n))) times))

 ;; ;; results : (listof benchmark-result?)
 ;; (define results (run-benchmarks benches
 ;;                                 (mk-bench-opts
 ;;                                  #:itrs-per-trial 1
 ;;                                  #:num-trials 30)))

 ;; ;; plot results
 ;; (parameterize ([plot-x-ticks no-ticks])
 ;;  (plot-pict #:x-label #f #:y-label "normalized time"
 ;;             (render-benchmark-alts (map fmt times) (fmt (car times)) results)))
 )

This illustrates a few features of this library.

The first is @(racket bench-one). We specify a @(racket
string?) (@(racket fmt n)) for the name of the benchmark and an
expression @(racket (sleep n)) for the expression to benchmark.

Second, we run the list of @(racket benchmark-one?) that we created,
using @(racket run-benchmarks), additionally supplying options for the
number of iterations per trial and the number of trials.  @(racket
#:itrs-per-trial) tells us the number of times to evaluate the
expression. In this case, we only need to evaluate @(racket (sleep n))
once. But for an expression such as @(racket (+ 2 3)), because the
time to evaluate is so small relative to timer resolution
@(racket (time (+ 2 3))) would report that it took 0 ms to
evaluate. Hence, we may instead want to see how long it takes to
evaluate @(racket (time (for ([i 100000]) (+ 2 3)))). In this case,
@(racket 100000) is our @(racket #:itrs-per-trial). The other option,
@(racket #:num-trials) represents the number of samples to gather. In
our case we use @(racket 30) so that we can use the normal
distribution to compute confidence intervals.

Third, we use @(racket render-benchmark-alts) to take the results that
we got from @(racket run-benchmraks) to comparisons of the individual
benchmarks.

And as we see, the normalized times of the benchmarks compare as we
expect.

@section[#:tag "single benchmark"]{Individual Benchmarks}

@defproc[(mk-bench-one [name string?]
                       [thunk procedure?]
                       [opts benchmark-opts? (mk-bench-opts)])
                       benchmark-one?]{
Produces a @(racket benchmark-one?) with associated @(racket name) for
computing the time to evaluate @(racket thunk)
@(racket (benchmark-opts itrs-per-trial)) times with specified
@(racket opts). This is the core structure of this library for
specifying how to run a benchmark.
}

@#reader scribble/comment-reader

(examples
 (require benchmark racket/function)
 
 (mk-bench-one "sleep .1" (thunk (sleep .1)))
 ;; gc takes a long time to run, so we need to only run it once
 ;; to get a reasonably-sized (~ 50 ms) time
 ;; also don't run gc between each trial, because that's what we
 ;; are trying to measure
 (mk-bench-one "gc" (thunk (collect-garbage))
               (mk-bench-opts #:gc-between #f
                              #:itrs-per-trial 1)))

@defform*[((bench-one name expr opts)
           (bench-one name expr)
           (bench-one expr))]{
The @(racket bench-one) macro is merely a wrapper for @(racket
mk-bench-one) to admit raw expressions where thunks are required in
@(racket mk-bench-one) and to derive a name from the expression if no
name is given.
}

Using the examples from @(racket mk-bench-one):

@#reader scribble/comment-reader

(examples
 (require benchmark racket/function)

 ;; don't need to specify name or use thunk
 ;; we see that in the benchmark-opts that the string form
 ;; of our expression "(sleep 0.1)" is used for the name
 (bench-one (sleep .1))

 ;; don't need to use thunk, but still need
 ;; to specify name as (bench-one expr opts) is not supported
 (bench-one "gc" (collect-garbage)
            (mk-bench-opts #:gc-between #f
                           #:itrs-per-trial 1)))

@defproc[(mk-shell-bench
          [name string?]
          [run (or/c string-no-nuls? bytes-no-nuls?)]
          [configure (or/c nothing? procedure? string-no-nuls? bytes-no-nuls?) nothing]
          [build (or/c nothing? procedure? string-no-nuls? bytes-no-nuls?) nothing]
          [extract-result (-> bytes? benchmark-trial-time?) racket-time-extract-result]
          [clean (or/c nothing? procedure? string-no-nuls? bytes-no-nuls?) nothing]
          [opts benchmark-opts? (mk-bench-opts #:itrs-per-trial 1
                                               #:manual-report-time #t)])
         benchmark-one?]{

Produces a @(racket benchmark-one?) with associated @(racket name) for
evaluating the time reported to evaluate @(racket (system run)).

Time must be reported by @(racket run) via stdout so that
that @(racket extract-result) can produce a @(racket
benchmark-trial-time?), hence @(racket #:manual-report-time #t).

Additionally, because @(racket run) is expected to report its
time, the default @(racket benchmark-opts?) used has @(racket
#:itrs-per-trial 1). That is, the trial time is precisely that
reported by @(racket run). In contrast, the default @(racket
#:itrs-per-trial 500) for @(racket mk-bench-one) is meant to
evaluate the thunk enough times so to minimize noise due to
clock tick resolution.
}

@defproc[(racket-time-extract-result [str bytes?])
         benchmark-trial-time?]{
To be used with @(racket #:extract-result) for the output of
Racket's @(racket time). Note: this is the default for the
@(racket #:extract-result) argument of @(racket mk-shell-benchmark).
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
benchmark-trial-time?) is set to @(racket 0). Please note that there
is also a time built-in for some shells, but isn't what is called when
using @(racket (system "time sleep 1")).
}

Suppose we have a file @(racket "a.rkt") in our current directory
that has the following contents:

@#reader scribble/comment-reader

@codeblock|{
  #lang racket
  
  ;; sleep for some time t \in [0, 1]
  ;; prints time to stdout in the form:
  ;; cpu time: 55 real time: 58 gc time: 55
  (time (sleep (random)))
  }|

@#reader scribble/comment-reader

(examples
 (require benchmark)

 ;; run 'racket a.rkt' using default for #:extract-result
 (mk-shell-bench
  "a.rkt"        ;; name of benchmark
  "racket a.rkt" ;; command to run using system
  )

 ;; use /usr/bin/time to benchmark /bin/sleep
 ;; specifying linux-time-extract-result to extract time from stdout
 (mk-shell-bench
  "/bin/sleep 5"
  "( /usr/bin/time -p sleep 5 ) 2>&1"
  #:extract-result linux-time-extract-result))

@defstruct[benchmark-trial-time? ([cpu real?]
                                  [real real?]
                                  [gc real?])
           #:prefab]{
Data structure for reporting the time of a single trial. This is
exposed to allow the user to write her own @(racket #:extract-time).
}

@section[#:tag "grouping benchmarks"]{Groups of Benchmarks}
@defproc[(mk-bench-group [name string?]
                         [benchmarks (listof (or/c benchmark-one? benchmark-group?))]
                         [opts benchmark-opts? (mk-bench-opts)])
         benchmark-group?]{}

Creates a @(racket benchmark-group?) with specified @(racket name)
for specified @(racket benchmarks). If @(racket opts) is not specified,
@(racket (mk-bench-opts)) is used, otherwise the user-specified options
are used. Groups serve two purposes: to recursively give names to
benchmarks and groups of benchmarks, and to allow convenient specification
of options for multiple benchmarks.

@#reader scribble/comment-reader
(examples
 (require benchmark)
 
 (define (fib n)
   (if (> 2 n) n (+ (fib (- n 1)) (fib (- n 2)))))

 (define benches
   (mk-bench-group
    "fibs" ;; group name
    (map
     (lambda (i) (bench-one (format "~a" i) (fib i)))
     (for/list ([i (in-range 25 30)]) i)) ;; (listof benchmark-one?)
    ))
 )

@section[#:tag "benchmark options"]{Options for Running Benchmarks}
@defproc[(mk-bench-opts [#:name name string? ""]
                        [#:gc-between gc-between (or/c boolean? nothing?) nothing]
                        [#:num-trials num-trials (or/c exact-integer? nothing?) nothing]
                        [#:itrs-per-trial itrs-per-trial(or/c exact-integer? nothing?) nothing]
                        [#:discard-first discard-first (or/c boolean? nothing?) nothing]
                        [#:manual-report-time manual-report-time (or/c boolean? nothing?) nothing])
         benchmark-opts?]{
Produces @(racket (benchmark-opts?)) used for configuring how benchmarks
at the @(racket benchmark-group?), @(racket benchmark-one?), or top-level are
run.
}

@subsection[#:tag "name"]{Name}
This option should not be manpiluated, as it is set by @(racket
bench-one), @(racket mk-bench-one), and @(racket mk-bench-group). The
purpose in its documentation here is to help understand how the
results of running benchmarks are named.

The keyword argument @(racket #:name) is used for naming @(racket
benchmark-one?) and @(racket benchmark-group?). Names for @(racket
benchmark-one?) can be thought of as file names and names for @(racket
benchmark-group?)  can be thought of as directory names. When a
@(racket benchmark-one?)  is run, the name associated with the results
is analogous to an absolute path.

@subsection[#:tag "gc-between"]{GC Between}
The keyword argument @(racket #:gc-between) specifies whether gc should be
run before each trial for the associated benchmark. If true, the GC is envoked
thrice before each trial, using @(racket (collect-garbage)).

@subsection[#:tag "num-trials"]{Number of Trials}
The keyword argument @(racket #:num-trials) specifies the number of trials
(samples) to gather for evaluating the thunk being benchmarked.

@subsection[#:tag "itrs-per-trial"]{Iterations per Trial}
The keyword argument @(racket #:itrs-per-trial) specifies the number of
times to evaluate the thunk during each trial. The purpose of this is to
guarantee that the thunk is evaluated enough times so that clock tick resolution
isn't significant.

@subsection[#:tag "discard-first"]{Discard First}
The keyword argument @(racket #:discard-first) determines whether the first
trial time is discarded. The purpose for this is to mitigate additional overhead
due to initialization.

@subsection[#:tag "manual-report-time"]{Manually Reporting Time}
;; TODO: update to reflect new name and semantics

By default, timing is performed using @(racket time-apply) and not the
responsibility of the consumer of this library. However, a consumer
may want additional control over what is being timed. When @(racket
#:manual-report-time) is set to @(racket #t), @(racket time-internal)
should be used to indicate the trial time. This is the mechanism used
for reporting times of shell benchmarks.

@section[#:tag "combining benchmarks"]{Combining Benchmarks and Their Options}

Benchmarks form a tree where @(racket benchmark-group?) are internal
nodes and @(racket benchmark-one?) are leafs. Both of these nodes have
@(racket benchmark-opts?) attached. For any node (internal or leaf) in
the tree, field @(racket f) of @(racket benchmark-opts?) is the value
of the @(racket f) for the @(racket benchmark-opts?) of that node if
it is not @(racket nothing?). Otherwise, the value for field @(racket
f) is inhereted from the parent node. The process is bootstrapped by
filling in fields with @(racket nothing?) values in the @(racket
benchmark-opts?)  of the root node with the defaults of @(racket
opts-default) prior to calling @(racket run-benchmarks).

@#reader scribble/comment-reader
(examples
 )

@section[#:tag "running benchmarks"]{Runing Benchmarks}
@defproc[(run-benchmarks [benchmarks (or/c benchmark-one? benchmark-group? (listof (or/c benchmark-one? benchmark-group?)))]
                         [opts (or/c benchmark-opts? nothing?) nothing])
         (listof benchmark-result?)]{}

@section[#:tag "Benchmark Times"]{Benchmark Times}
@defproc[(time-internal [thunk procedure?])
          void?]{
    When uesd with @(racket #:manual-report-time #f) in @(racket benchmark-opts?),
    reports the time required to evaluate @(racket (thunk)).
}

@section[#:tag "Plotting"]{Plotting}
@(racket benchmark) exports a @(racket renderer2d?) for plotting results
of benchmarks.
@defproc[(render-benchmark-alts [alt-names (listof string?)]
                                [norm-alt-name string?]
                                [benchmark-results (listof benchmark-result?)])
         renderer2d?]{
    Produces a @(racket renderer2d?) for the specified benchmark results with
    error bars.
    }

@section[#:tag "Persisting Results"]{Persisting Results}
Results can be persisted with @(racket record-results) and retrieved
with @(racket get-past-results).

@defproc[(record-results [results any/c]
                         [file path?])
         void?]
Persists @(racket results) to file-<n> where <n> is the smallest natural
such that file-<n> doesn't exist.

@defproc[(get-past-results [file path?]
                           [version (or/c exact-integer? #f) #f])
         any/c]{
If @(racket version) is specified by user, reads @(racket bench-results?) from
file-version. Otherwise, reads @(racket bench-results?) from file-<n> where
n is the largest natural such that file-<n> does not exist.
}

@defstruct[bench-results ([results any/c]
                          [time/date date*]) #:prefab]{
Base struct for persisting results to files.
}

@defstruct[(linux-bench-results bench-results) ([hostname string?]
                                                [uname string?])
           #:prefab]{
Extension of @(racket bench-results) struct for persisting additional
info of Linux uname and hostname. The most convenient way to get this
information is using @(racket attach-linux-info).
}

@defproc[(attach-linux-info [results any/c]) linux-bench-result?]{
Injects @(racket results) into @(racket linux-bench-result?) after
getting uname, hostname, and date/time.
}

@defproc[(attach-date/time [results any/c]) linux-bench-result?]{
Injects @(racket results) into @(racket bench-result?) after
getting date/time.
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

@;; Local Variables:
@;; compile-command: "raco setup benchmark"
@;; End:
