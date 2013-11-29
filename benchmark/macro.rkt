#lang racket

(provide run-macro-benchmarks
         racket-time-extract-result
         linux-time-extract-result)

(require racket/format)
(require "types.rkt" "stats.rkt")

(define benchmark-logger (make-logger 'benchmark (current-logger)))

(define (run-macro-benchmarks
         whats ;; (listof string?)
         hows  ;; (listof (listof any/c))
         run   ;; (any/c ... -> void?)
         #:build [build #f]  ;; (any/c ... -> void?)
         #:clean [clean #f]  ;; (any/c ... -> void?)
         ;; string -> benchmark-trial-time?
         #:extract-time [extract-time racket-time-extract-result]
         #:num-trials [num-trials 30] ;; exact-integer?
         ;; (any/c ... -> string?)
         #:make-name [make-name (lambda (x) x)])
  (define (build-run-clean-1 opts)
    (define what (car opts))
    (define how (cdr opts))
    (define (run-1)
      (let* ([out (open-output-string)]
             [delta-run-time
              (parameterize ([current-output-port out])
                (time-delta (thunk (apply run opts))))]
             [extracted-run-time
              (extract-time (get-output-string out))])
        (log-message benchmark-logger 'info "running" opts)
        extracted-run-time))
    (define out (open-output-string))
    (define delta-build-time
      (if build
          (begin
            (log-message benchmark-logger 'info "building" opts)
            (time-delta (thunk (apply build opts))))
          0))
    (define run-times (for/list ([i num-trials]) (run-1)))
    (define delta-clean-time
      (if clean
          (begin
            (log-message benchmark-logger 'info "cleaning" opts)
            (time-delta (thunk (apply clean opts))))
          0))
    (benchmark-result
     (make-name what)
     how
     (raw-to-stats run-times)))
  (map build-run-clean-1 (cartesian-product (cons whats hows))))

(define (cartesian-product ls)
  (define (cp-2 as bs)
    (for*/list ([i (in-list as)] [j (in-list bs)]) (cons i j)))
  (foldr cp-2 (list (list)) ls))

(define (racket-time-extract-result str)
  (let* ([m (regexp-match #rx#"cpu time: ([0-9]+) real time: ([0-9]+) gc time: ([0-9]+)" str)])
    (if (not m)
        (error (format "Could not parse time output: ~a" str))
        (benchmark-trial-time
         (bytes->number (cadr m))
         (bytes->number (caddr m))
         (bytes->number (cadddr m))))))

;; linux-time-extract-result : bytes? ->  benchmark-trial-time?
;; for use with /usr/bin/time -p (POSIX standard 1003.2)
(define (linux-time-extract-result str)
  (let ([m (regexp-match #rx#"real ([0-9.]+)" str)])
    (if (not m)
        (error (format "Could not parse linux time output: ~a" str))
        (let ([msecs (* 1000 (bytes->number (cadr m)))])
          (benchmark-trial-time
           msecs ;; cpu time
           msecs ;; real time
           0)))))   ;; gc time

;; time-delta : procedure? -> real?
(define (time-delta thunk)
  (let ([start (current-inexact-milliseconds)])
    (thunk)
    (let ([end (current-inexact-milliseconds)])
      (- end start))))

;; bytes? -> number?
(define (bytes->number b)
  (string->number (bytes->string/latin-1 b)))
