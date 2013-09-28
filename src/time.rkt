#lang racket

(require "types.rkt")

(provide
 report-time
 receive-time
 time-internal
 )

;; for internal timing
(define timing-logger (make-logger 'timing-logger))
(define timing-logger-receiver (make-log-receiver timing-logger 'info))
(define (report-time time) (log-message timing-logger 'info "" time))

;; procedure? -> void
(define (time-internal thunk)
  (let-values ([(_ cpu real gc) (time-apply thunk '())])
    (report-time (benchmark-trial-time cpu real gc))))

;; benchmark-trial-time?
(define (receive-time)
  (define timeout .01)
  (let ([trial-time (sync timing-logger-receiver)])
    (discard-extra-times timeout)
    (if (not trial-time)
        (error (format "No log message received within ~a s, exiting." timeout))
        (vector-ref trial-time 2))))

;; num? -> void
(define (discard-extra-times timeout)
  (when (sync/timeout timeout timing-logger-receiver)
    (discard-extra-times timeout)))
