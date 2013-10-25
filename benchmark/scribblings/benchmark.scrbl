#lang scribble/manual

@(require (for-label racket plot racket/set) scribble/eval)

@title[#:tag "top"]{Benchmarking}
@author{@(author+email "Josh McGrath" "mcgrathj@ccs.neu.edu")}

@defmodule[benchmark]

The goal of the benchmark library is to reduce the effort of writing
benchmarks for comparing alternatives. In the simplest case, only names
of specific benchmarks, names of containing groups, and expressions
must be specified. At the same time, control over gc, number of times run,
what precisely is timed, are exposed in case additional configuration is
required. Currently this library is in a pre-alpha state.

@table-of-contents[]

@section[#:tag "simple example"]{A Simple Example}
To gain some confidence in our benchmarking framework, let's begin by
benchmarking Racket's 

@#reader scribble/comment-reader

(examples
 (require plot benchmark)
 
 ;; fmt : num? -> string?
 ;; format benchmark name
 (define (fmt n) (format "sleep ~a" n))

 ;; sleep-internal-bench : num? -> benchmark-one?
 (define (sleep-internal-bench n) (bench-one (fmt n) (sleep n)))

 ;; times : (listof num?)
 ;; times to sleep for
 (define times (list .1 .2 .3))

 ;; benches : (listof benchmark-one?)
 (define benches (map sleep-internal-bench times))

 ;; results : (listof benchmark-result?)
 ;; (define results (run-benchmarks benches
 ;;                                 (mk-bench-opts
 ;;                                  #:itrs-per-trial 1
 ;;                                  #:num-trials 30)))

 ;; record results in a file
 
 ;; plot results
 ;; (parameterize ([plot-x-ticks no-ticks])
 ;;  (plot-pict #:x-label #f #:y-label "normalized time"
 ;;             (render-benchmark-alts (map fmt times) (fmt (car times)) results)))
 )

Phew! The rest of the document is structured as a reference for the
identifiers exposed.

@section[#:tag "single benchmark"]{Individual Benchmarks}

@defproc[(mk-bench-one [name string?]
                       [thunk procedure?]
                       [opts benchmark-opts? (mk-bench-opts)])
                       benchmark-one?]{
    Produces a @(racket benchmark-one?) with associated @(racket name) for
    computing the time to evaluate @(racket thunk)
    @(racket (benchmark-opts itrs-per-trial)) times with specified
    @(racket opts).
    }

@defproc[(mk-shell-bench [name string?]
                         [run (or/c procedure? string-no-nuls? bytes-no-nuls?)]
                         [configure (or/c procedure? string-no-nuls? bytes-no-nuls?) nothing]
                         [build (or/c procedure? string-no-nuls? bytes-no-nuls?) nothing]
                         [extract-result (-> bytes? benchmark-trial-time?) default-extract-result]
                         [clean (or/c procedure? string-no-nuls? bytes-no-nuls?) nothing]
                         [opts benchmark-opts? (mk-bench-opts #:itrs-per-trial 1
                                                      #:time-external #f)])
         benchmark-one?]{
    Produces a @(racket benchmark-one?) with associated @(racket name) for
    the time @bold{reported} to evaluate @(racket (run)) if @(racket run) is a
    @(racket procedure?) or @(racket (system run)) otherwise. Time must be
    reported by @(racket run) via stdout in such a way that @(racket extract-result)
    can produce a @(racket benchmark-trial-time?). Because @(racket run) is
    expected to report its time, the default @(racket bench-opts?) used
    has @(racket #:itrs-per-trial 1). That is, the trial reported by
    @(racket run) is precisely the time for that trial. In contrast, the default
    @(racket #:itrs-per-trial 500) for @(racket mk-bench-one)
    evaluates the thunk enough times so to minimize noise due to clock
    tick resolution.}

@defproc[(linux-time-extract-result [str bytes?])
         benchmark-trial-time?]{
To be used with @(racket #:extract-result) for the output of
@(shell /usr/bin/time -p) (i.e. POSIX standard 1003.2). Parses
@(shell /usr/bin/time -p) output and sets @(racket cpu) and @(racket real)
fields of @(benchmark-trial-time?) to "user" time reported by
@(shell /usr/bin/time -p). The @(racket gc) of @(benchmark-trial-time?) is
set to @(racket 0).
}

@defstruct[benchmark-trial-time? ([cpu real?]
                                  [real real?]
                                  [gc real?])
           #:prefab]{
Data structure for reporting the time of a single trial.
}

@defform*[((bench-one name expr opts)
                    (bench-one name expr)
                    (bench-one expr))]{
    Produces a @(racket benchmark-one?) for the supplied @(racket expr).
    If @(racket name) is not provided, the string form of @(racket expr) is
    used as the name. If @(racket name) is provided, it is expected to be a
    string. If @(racket opts) is not provided,
    @(racket (mk-bench-opts is used)). If @(racket opts) is provided, it is
    expected to have type @(racket benchmark-opts?).
    }

@section[#:tag "grouping benchmarks"]{Groups of Benchmarks}
@defproc[(mk-bench-group [name string?]
                                              [benchmarks (listof (or/c benchmark-one? benchmark-group?))]
                                              [opts benchmark-opts? (mk-bench-opts)])
         benchmark-group?]{}

Creates a @(racket benchmark-group?) with specified @(racket name)
for specified @(racket benchmarks). If @(racket opts) is not specified,
@(racket (mk-bench-opts)) is used, otherwise the user-specified options
are used.

@section[#:tag "benchmark options"]{Options for Running Benchmarks}
@defproc[(mk-bench-opts [#:name name string? ""]
                        [#:gc-between gc-between (or/c boolean? nothing?) nothing]
                        [#:num-trials num-trials (or/c exact-integer? nothing?) nothing]
                        [#:itrs-per-trial itrs-per-trial(or/c exact-integer? nothing?) nothing]
                        [#:discard-first discard-first (or/c boolean? nothing?) nothing]
                        [#:time-external time-external (or/c boolean? nothing?) nothing])
         benchmark-opts?]{
    Produces @(racket (benchmark-opts?)) used for configuring how benchmarks,
    at the @(racket benchmark-group?) @(racket benchmark-one?) or top-level are
    run.
    }

@subsection[#:tag "combining benchmark options"]{Combining Benchmark Options}
All fields of options, with the exception of @(racket #:name), combine in the
following way: given @(racket o1) and @(racket o2) of type
@(racket benchmark-opts?)

@subsection[#:tag "name"]{Name}
The keyword argument @(racket #:name) is used for naming @(racket benchmark-one?) and
@(racket benchmark-one?). Names for @(racket benchmark-one?) can
be thought of as file names and names for @(racket benchmark-group?)
can be thought of as directory names. When a @(racket benchmark-one?)
is run, the name associated with the results is analogous to an
absolute path.

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

@subsection[#:tag "time-external"]{Time External}
The two forms of timing suppored at "internal" and "external". External timing
is the default, and is the end-to-end time to evaluate a thunk using
@(racket time-apply). On the other hand, a consumer of this library may
want additional control over what precisely is being timed. When
@(racket #:time-external) is set to @(racket #f), the consumer may use
@(racket time-internal) to indicate the trial time.

@section[#:tag "running benchmarks"]{Runing Benchmarks}
@defproc[(run-benchmarks [benchmarks (or/c benchmark-one? benchmark-group? (listof (or/c benchmark-one? benchmark-group?)))]
                         [opts (or/c benchmark-opts? nothing?) nothing])
         (listof benchmark-result?)]{}

@section[#:tag "Benchmark Times"]{Benchmark Times}
@defproc[(time-internal [thunk procedure?])
          void?]{
    When uesd with @(racket #:time-external #f) in @(racket benchmark-opts?),
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
Extension of @(racket bench-results) struct for persisting additional info
of Linux uname and hostname. The most convenient way to get this information
is using @(racket attach-linux-info).
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
