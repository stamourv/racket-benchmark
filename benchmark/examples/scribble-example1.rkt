#lang racket

(require benchmark plot racket racket/runtime-path compiler/find-exe)

  (define-runtime-path fib-path
    "macro-examples/fib.rkt")
  (define-runtime-path collatz-path
    "macro-examples/collatz.rkt")
  (define-runtime-path compiled-dir
    "macro-examples/compiled")

  (define results
    (run-benchmarks
     ;; files to run (whats)
     (list fib-path collatz-path)
     ;; list of options (hows)
     (list (list 'jit 'no-jit))
     ;; how to run each benchmark
     (lambda (file jit)
       (if (equal? jit 'jit)
           (system* (find-exe) file)
           (system* (find-exe) "-j" file)))
     #:build
     (lambda (file jit)
       (system* (find-exe) "-l" "raco" "make" file))
     #:clean
     (lambda (file jit)
       (system* (find-executable-path "rm") "-r" "-f" compiled-dir))
     #:num-trials 2
     #:make-name (lambda (path)
                   (let-values
                       ([(_1 file-name _2) (split-path path)])
                     (path->string file-name)))))

  results

  (parameterize ([plot-x-ticks no-ticks])
    (plot-pict
     #:title "jit vs no jit"
     #:x-label #f
     #:y-label "normalized time"
     (render-benchmark-alts
      ;; default options
      (list 'jit)
      results)))
