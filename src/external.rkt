#lang racket

(require "types.rkt")
(require "time.rkt")
(require racket/system)

(provide
 mk-shell-benchmark
 time-shell-cmd)

;; command : (or/c string-no-nuls? bytes-no-nuls?)

;; shell-benchmark? -> shell-benchmark-trial-time?
(define (time-shell-cmd b)
  (let* ([out (open-output-bytes)]
         [configure-time (maybe-execute-cmd (shell-benchmark-configure b))]
         [build-time (maybe-execute-cmd (shell-benchmark-build b))]
         [run-times
          (get-run-times
           (shell-benchmark-run b)
           (shell-benchmark-extract-result b))]
         [clean-time (maybe-execute-cmd (shell-benchmark-clean b))]
         [btt (cdr run-times)]
         [run-time (car run-times)])
    (shell-benchmark-trial-time
     (benchmark-trial-time-cpu btt)
     (benchmark-trial-time-real btt)
     (benchmark-trial-time-gc btt)
     configure-time
     build-time
     run-time
     clean-time)))

;; (or/c command procedure? nothing?) -> (or/c flonum? nothing?)
(define (maybe-execute-cmd action)
  (if (nothing? action)
      nothing
      (time-delta
       (if (procedure? action)
           (action)
           (system action)))))

;; command -> (flonum? . benchmark-trial-time?)
(define (get-run-times action extract-times)
  (let* ([out (open-output-bytes)]
         [run-time
          (parameterize
              ([current-output-port out]
               [current-error-port out])
            (time-delta
             (lambda ()
               (unless (system action)
                 (error "~a failed with output: ~a" action (get-output-bytes out))))))])
    (cons run-time (extract-times (get-output-bytes out)))))

;; procedure? -> flonum?
(define (time-delta thunk)
  (let ([start (current-inexact-milliseconds)])
    (thunk)
    (let ([end (current-inexact-milliseconds)])
      (- end start))))

;; command : (or/c string-no-nuls? bytes-no-nuls?)

(define (mk-shell-benchmark
         name                                    ;; string?
         run                                     ;; command
         #:configure [configure nothing]         ;; command
         #:build [build nothing]                 ;; command
         #:extract-result [extract-result default-extract-result] ;; ??
         #:clean [clean nothing]                 ;; command
         #:opts [opts (mk-benchmark-opts
                       #:itrs-per-trial 1
                       #:time-external #f)]) ;; benchmark-opts?
  (mk-benchmark-one
   name
   (lambda ()
     (report-time (time-shell-cmd
                   (shell-benchmark
                    nothing
                    opts
                    configure
                    build
                    run
                    extract-result
                    clean))))
   opts))

;; string? -> benchmark-trial-time?
(define (default-extract-result str)
  (let* ([m (regexp-match #rx#"cpu time: ([0-9]+) real time: ([0-9]+) gc time: ([0-9]+)" str)])
    (if (not m)
        (error (format "Could not parse time output: ~a" str))
        (benchmark-trial-time
         (bytes->number (cadr m))
         (bytes->number (caddr m))
         (bytes->number (cadddr m))))))

(define (bytes->number b)
  (string->number (bytes->string/latin-1 b)))
