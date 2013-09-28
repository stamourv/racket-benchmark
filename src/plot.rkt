#lang racket

(require "types.rkt")
(require plot)

(provide plot-benchmark-result)

;; list? (benchmark-result? . ) -> void
(define (plot-benchmark-result br)
  (let* ([bts (benchmark-result-trial-stats br)]
         [opts (benchmark-result-opts br)]
         [cpus (benchmark-trial-stats-cpu bts)]
         [reals (benchmark-trial-stats-real bts)]
         [gcs (benchmark-trial-stats-gc bts)])
    (parameterize ([plot-legend-anchor 'top-right]
                   [plot-title (benchmark-opts-name opts)]
                   [plot-x-label "time (ms)"]
                   [plot-y-label "probability density function"])
      (plot-file
       (if (andmap zero? (measured-value-samples gcs))
           (list
            (render-vals cpus "cpu" opts)
            (render-vals reals "real" opts))
           (list
            (render-vals cpus "cpu" opts)
            (render-vals reals "real" opts)
            (render-vals gcs "gc" opts)))
       (let ([out-file (if (benchmark-opts-plot-file opts)
                           (benchmark-opts-plot-file opts)
                           "default.pdf")])
         (displayln
          (format "Writing output for ~a to ~a" (benchmark-opts-name opts) out-file))
         out-file)
       'pdf))))

(define (render-vals mv n opts)
  (density
   (measured-value-samples mv)
   #:y-min 0
   #:color (cond [(equal? "real" n) 0]
                 [(equal? "cpu" n) 40]
                 [(equal? "gc" n) 80])
   #:label (format "~a ~a" n (benchmark-opts-name  opts))))
