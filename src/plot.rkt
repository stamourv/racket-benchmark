#lang racket

(require "types.rkt")
(require "util.rkt")
(require plot)

(provide plot-benchmarks)

(define plots-dir "plots")

(define (plot-benchmarks names brs title file-name)
  (let ([brs-to-plot (select-results-by-name
                      (if (list? names) names (list names))
                      brs)])
    (plot-benchmarks-aux brs-to-plot title file-name)))

;; benchmark-plot? -> void
(define (plot-benchmarks-aux brs title file-name)
  (let* ([bts (map benchmark-result-trial-stats brs)]
         [out-file
          (lambda (t) (format "plots/~a-~a" (symbol->string t) file-name))])
    (maybe-mkdir plots-dir)
    (parameterize ([plot-legend-anchor 'top-right]
                   [plot-x-label "time (ms)"]
                   [plot-y-label "probability density function"])
      (for-each
       (lambda (t)
         (parameterize ([plot-title (format "~a (~a)" title (symbol->string t))])
           (let ([renderers
                  (filter-map
                   (lambda (br c) (render t br c))
                   brs
                   (color-value-list (length brs)))])
             (unless (null? renderers)
               (format "Writing output for ~a/~a to ~a" title t out-file)
               (plot-file renderers (out-file t) 'pdf)))))
       (list 'real 'cpu 'gc)))))

(define (render type br color)
  (define opts (benchmark-result-opts br))
  (define (render-aux mv)
    (density
     (measured-value-samples mv)
     #:color color
     #:y-min 0
     #:label (format "~a" (benchmark-opts-name opts))))
  (let* ([bts (benchmark-result-trial-stats br)]
         [opts (benchmark-result-opts br)]
         [cpus (benchmark-trial-stats-cpu bts)]
         [reals (benchmark-trial-stats-real bts)]
         [gcs (benchmark-trial-stats-gc bts)])
    (cond [(equal? type 'gc)
           (if (andmap zero? (measured-value-samples gcs))
               #f
               (render-aux gcs))]
          [(equal? type 'cpu) (render-aux cpus)]
          [(equal? type 'real) (render-aux reals)])))

;; list? (string?) list? (benchmark-result?)-> 
(define (select-results-by-name names results)
  (filter (lambda (r)
            (member (benchmark-opts-name
                     (benchmark-result-opts r))
                    names))
          results))

(define (color-value-list num-colors)
  (define min-color 0)
  (define max-color 150) ;; TODO: what should this value be?
  (define inc (truncate (/ (- max-color min-color) (- num-colors 1))))
  (for/list ([i num-colors]) (* i inc)))
