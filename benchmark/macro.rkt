#lang racket

(provide run-benchmarks
         racket-time-extract-result)

(require racket/format racket/serialize)
(require "types.rkt" "results.rkt")

(define (run-benchmarks
         whats ;; (listof string?)
         hows  ;; (listof (listof any/c))
         run   ;; (any/c ... -> void?)
         #:build [build #f]  ;; (any/c ... -> void?)
         #:clean [clean #f]  ;; (any/c ... -> void?)
         ;; (or/c 'delta-time (string -> real?))
         #:extract-time [extract-time racket-time-extract-result]
         #:num-trials [num-trials 30] ;; exact-integer?
         ;; (any/c ... -> string?)
         #:make-name [make-name (lambda (x) x)]
         #:skip [skip (lambda (r . rest) #f)]
         #:results-file [file-base #f])
  (define filename (and file-base (make-fresh-file-name file-base)))
  (define (build-run-clean-1 opts)
    (define what (car opts))
    (define how (cdr opts))
    (define (run-1)
      (log-message benchmark-logger 'info (~a "running " opts) opts)
      (let* ([out (open-output-string)]
             [delta-run-time
              (parameterize ([current-output-port out])
                (time-delta (thunk (apply run opts))))]
             [extracted-run-time
              (if (equal? extract-time 'delta-time)
                  delta-run-time
                  (extract-time (get-output-string out)))])
        extracted-run-time))
   (define out (open-output-string))
    (define delta-build-time
      (if build
          (begin
            (log-message benchmark-logger 'info (~a "building " opts) opts)
            (time-delta (thunk (apply build opts))))
          0))
    (define run-times (for/list ([i num-trials]) (run-1)))
    (define delta-clean-time
      (if clean
          (begin
            (log-message benchmark-logger 'info (~a "cleaning " opts) opts)
            (time-delta (thunk (apply clean opts))))
          0))
    (define result
      (benchmark-result
       (make-name what)
       how
       run-times))
    (when filename ; record intermediate results?
      ;; TODO be able to print results even when a set of runs is not complete
      (with-output-to-file
          filename #:exists 'append
        (lambda () (write (serialize result)) (newline))))
    result)
  (map build-run-clean-1
       (filter (lambda (b) (not (apply skip b)))
               (apply cartesian-product (cons whats hows)))))

(define (racket-time-extract-result str)
  (let* ([m (regexp-match
             #rx"cpu time: (-?[0-9]+) real time: (-?[0-9]+) gc time: (-?[0-9]+)"
             str)])
    (if (not m)
        (error (format "Could not parse time output: ~a" str))
        (string->number (caddr m)))))

;; time-delta : procedure? -> real?
(define (time-delta thunk)
  (let ([start (current-inexact-milliseconds)])
    (thunk)
    (let ([end (current-inexact-milliseconds)])
      (- end start))))
