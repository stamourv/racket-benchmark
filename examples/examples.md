## fib-collatz.rkt ##
- calculating fibonacci, collatz for various values
- simple use of b1 bgroup macros
- b1 without name (using expression string as name)

## fusion.rkt ##
- calculating (add1 . square . sqrt) over a list wit and without fusion.
- mk-bgroup and mk-b1 for dynamic creation of benchmarks
- plots result differences in fusion-no-fusion.pdf

## internal-external.rkt ##
- usage of time-internal macro to allow for setup
- comparison of internal and external timing

## jit-no-jit.rkt ##
- shell benchmark
- running racket on external files both with and without jit

## optimization-coach.rkt ##
- interface to typed racket
- dynamically creating benchmarks

## ping.rkt ##
- comparing multiple trials to each other
- shell benchmark (of a regular shell command)
- reading time from output of ping
- plotting results to ping.pdf
