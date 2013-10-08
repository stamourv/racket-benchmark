#lang racket

(require "types.rkt" "util.rkt")

(require plot plot/utils srfi/13)

(provide render-benchmark-alts)

(define (render-benchmark-alts alt-names brs)
  (define (select-benchmarks alt-name)
    (define alt-name-pref (string-append alt-name "/"))
    (filter-map
     (lambda (br)
       (let* ([opts (benchmark-result-opts br)]
              [name (benchmark-opts-name opts)])
         (if (string-prefix? alt-name-pref name)
             (let ([new-name (substring name (string-length alt-name-pref))]
                   [trial-stats (benchmark-result-trial-stats br)])
               ;; update name of benchmark by removing alt-name-pref
               (mk-benchmark-result
                (struct-copy benchmark-opts opts [name new-name])
                trial-stats))
             #f)))
     brs))
  (define num-alts (length alt-names))
  (define alt-nums (for/list ([i num-alts]) i))
  (define colors (color-seq "red" "green" num-alts))
  (define start-xs (linear-seq 0 (- num-alts 1) num-alts))
  (define skip (+ num-alts 1))
  (define bar-width (- (/ skip (+ num-alts 1)) (discrete-histogram-gap)))
  (append-map (lambda (an c x)
                (render-benchmark-alt an (select-benchmarks an) c x skip bar-width))
              alt-names
              colors
              start-xs))

;; render the histogram of an alternative
(define (render-benchmark-alt alt-name brs color start-x skip bar-width
                    #:type-sel [type-sel benchmark-trial-stats-real])
  (define (data-point br)
    (let* ([opts (benchmark-result-opts br)]
           [data-name (benchmark-opts-name opts)]
           [mv (type-sel (benchmark-result-trial-stats br))]
           [mean (measured-value-mean mv)])
      (vector data-name mean)))
  (define (data-error-bars br i)
    (let* ([mv (type-sel (benchmark-result-trial-stats br))]
           [conf-lb (measured-value-conf-lb mv)]
           [conf-ub (measured-value-conf-ub mv)]
           [conf-mean (/ (+ conf-lb conf-ub) 2)]
           [conf-ht (- conf-ub conf-mean)]
           [delta-to-mid-bar (/ (+ bar-width (discrete-histogram-gap)) 2)])
      (parameterize ([plot-x-ticks no-ticks])
        (error-bars (list (vector
                           (+ start-x (* skip i) delta-to-mid-bar)
                           conf-mean
                           conf-ht))
                    #:line-width 1))))
  (cons   
   (discrete-histogram (map data-point brs)
                       #:skip skip
                       #:label alt-name                      
                       #:color color
                       #:x-min start-x)
   (map data-error-bars brs (for/list ([i (length brs)]) i))))

