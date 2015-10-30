#lang racket

; JIT example
(require benchmark plot/pict racket racket/runtime-path compiler/find-exe)

(define-runtime-path fib-path
  "macro-examples/fib.rkt")
(define-runtime-path collatz-path
  "macro-examples/collatz.rkt")
(define-runtime-path compiled-dir
  "macro-examples/compiled")
(define-runtime-path jit-results-file
  "docs-jit-results")
(define-runtime-path list-vector-results-file
  "docs-list-vector-results")

(define jit-results
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
   #:num-trials 30
   #:make-name (lambda (path)
                 (let-values
                     ([(_1 file-name _2) (split-path path)])
                   (path->string file-name)))))

(record-results jit-results jit-results-file)

(parameterize ([plot-x-ticks no-ticks])
  (plot-pict
   #:title "jit vs no jit"
   #:x-label #f
   #:y-label "normalized time"
   (render-benchmark-alts
    ;; default options
    (list 'jit)
    jit-results)))


; Vector/List example
(require benchmark plot/pict racket/match racket/vector racket/list)

;; list/vector sizes
(define sizes (list 50000 100000))

(define lists (map (lambda (i) (range i)) sizes))

(define vectors (map list->vector lists))

(define list-vector-results
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

(record-results list-vector-results list-vector-results-file)

(parameterize ([plot-x-ticks no-ticks])
  (plot-pict
   #:title "vectors vs lists"
   #:x-label #f
   #:y-label "normalized time"
   (render-benchmark-alts
    ;; default options
    (list (cons 0 50000) 'list)
    list-vector-results
    ;; format options so we can omit the index in the size list
    #:format-opts (lambda (opts)
                    (let ([index/size (car opts)]
                          [impl (cadr opts)])
                      (format "size: ~a, ~a" (cdr index/size) impl))))))
