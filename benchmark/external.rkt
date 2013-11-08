#lang racket

(require "time.rkt" "types.rkt" (for-syntax "types.rkt"))
(require racket/system)

(provide
 make-shell-bench
 make-racket-file-bench
 time-shell-cmd
 racket-time-extract-result
 linux-time-extract-result)

;; command : (or/c string-no-nuls? bytes-no-nuls?)

;; time-shell-cmd : shell-benchmark? -> shell-benchmark-trial-time?
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

;; maybe-execute-cmd : (or/c command procedure? #f?)
;;                      -> (or/c real? #f?)
(define (maybe-execute-cmd action)
  (if (not action)
      #f
      (time-delta
       (thunk
        (unless (if (procedure? action)
                    (action)
                    (system action))
          (error (format "Failed running ~a\n" action)))))))

;; get-run-times : command (bytes? -> benchmark-trial-time?)
;;                 -> (real? . benchmark-trial-time?)
(define (get-run-times action extract-times)
  (let* ([out (open-output-bytes)]
         [run-time
          (parameterize
              ([current-output-port out])
            (and
             ;; TODO: use contracts
             action
             (not (procedure? action))
             (maybe-execute-cmd action)))])
    (cons run-time (extract-times (get-output-bytes out)))))

;; time-delta : procedure? -> real?
(define (time-delta thunk)
  (let ([start (current-inexact-milliseconds)])
    (thunk)
    (let ([end (current-inexact-milliseconds)])
      (- end start))))

;; m-command-or-proc : (or/c command procedure? #f)

;; make-shell-bench : string? m-command-or-proc -> benchmark-one?
(define (make-shell-bench
         name
         run
         #:configure [configure #f]         ;; m-command-or-proc
         #:build [build #f]                 ;; m-command-or-proc
         #:extract-result
         [extract-result
          racket-time-extract-result] ;; (bytes? -> benchmark-trial-time?)
         #:clean [clean #f]                 ;; m-command-or-proc
         #:num-trials [num-trials (num-trials)]             ;; exact-integer?
         #:discard-first [discard-first (discard-first)]    ;; boolean?
         #:opts [opts (benchmark-opts
                       (gc-between)
                       num-trials
                       (itrs-per-trial)
                       discard-first
                       (manual-report-time))])
  (make-bench-one
   name
   (thunk
    (report-time (time-shell-cmd
                  (shell-benchmark
                   configure
                   build
                   run
                   extract-result
                   clean))))
   #:opts (struct-copy benchmark-opts opts
                       [itrs-per-trial 1]
                       [manual-report-time #t])))

;; make-racket-file-bench : string? string? (listof string?) -> benchmark-one?
(define (make-racket-file-bench
         name
         fname
         args
         #:configure [configure #f]               ;; m-comand-or-proc
         #:build [build (format "raco make ~a" fname)] ;; m-command-or-proc
         #:extract-result
         [extract-result
          racket-time-extract-result]             ;; (bytes-> benchmark-trial-time?)
         #:clean [clean #f]              ;; m-command-or-proc
         #:num-trials [num-trials (num-trials)]             ;; exact-integer?
         #:discard-first [discard-first (discard-first)]    ;; boolean?
         #:opts [opts (benchmark-opts
                       (gc-between)
                       num-trials
                       (itrs-per-trial)
                       discard-first
                       (manual-report-time))])
  (make-shell-bench
   name
   (string-join (cons "racket" (append args (list fname))) " ")
   #:configure configure
   #:build build
   #:extract-result extract-result
   #:clean clean
   #:opts opts))

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

;; bytes? -> number?
(define (bytes->number b)
  (string->number (bytes->string/latin-1 b)))
