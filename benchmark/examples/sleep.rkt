#lang racket

(provide run plot-results)

(require benchmark plot)

(define times
  (list .00001 .0001 .001 .01 .1 .25 .5))

(define multipliers
  (list (list 1 3 4)))

(define (run extract-time)
  (run-benchmarks
   times
   multipliers
   (lambda (sleep-time multiplier)
     (let* ([noise-sign (if (> 1 (random 2)) 1 (- 1))]
            [noise (* 0.05 noise-sign (random) sleep-time multiplier)])
       (time (sleep (+ noise (* sleep-time multiplier))))))
   #:extract-time extract-time
   #:num-trials 30))

(define (plot-results results file-name)
  (parameterize ([plot-x-ticks no-ticks])
    (plot-file
     (render-benchmark-alts
      '(1)
      results
      #:format-opts (lambda (m) (format "multiplier ~a" m)))
     file-name
     #:title "sleep"
     #:x-label #f
     #:y-label "normalized time")))
