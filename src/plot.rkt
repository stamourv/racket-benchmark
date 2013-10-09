#lang racket

(require "types.rkt" "util.rkt" "stats.rkt")

(require plot plot/utils srfi/13)

(provide render-benchmark-alts)

(define (normalize-br norm-br br)
  (define (norm-val fn)
    (define norm-mean
      (measured-value-mean (fn (benchmark-result-trial-stats norm-br))))
    (if (zero? norm-mean)
        +inf.0
        (/ 1 norm-mean)))
  (define (samples fn)
    (measured-value-samples (fn (benchmark-result-trial-stats br))))
  (define norm-cpu-val (norm-val benchmark-trial-stats-cpu))
  (define norm-real-val (norm-val benchmark-trial-stats-real))
  (define norm-gc-val (norm-val benchmark-trial-stats-gc))
  (define normd-cpu-samples
    (map (lambda (s) (* s norm-cpu-val))
         (samples benchmark-trial-stats-cpu)))
  (define normd-real-samples
    (map (lambda (s) (* s norm-real-val))
         (samples benchmark-trial-stats-real)))
  (define normd-gc-samples
    (map (lambda (s) (* s norm-gc-val))
         (samples benchmark-trial-stats-gc)))
  (benchmark-result
   (benchmark-result-opts br)
   (raw-to-stats (map benchmark-trial-time
                      normd-cpu-samples
                      normd-real-samples
                      normd-gc-samples))))

(define (render-benchmark-alts alt-names brs norm-alt-name)
  (define (select-benchmarks alt-name)
    (define alt-name-pref (string-append alt-name "/"))
    (define (br-name br)
      (benchmark-opts-name (benchmark-result-opts br)))
    (define (norm-list br-suff)
      (filter (lambda (br) (equal?
                            (br-name br)
                            (string-append norm-alt-name "/" br-suff)))
              brs))
    (define (norm-br br-suff)
      (define nl (norm-list br-suff))
      (cond [(null? nl)
             (error (format "Benchmark ~a not found" norm-alt-name))]
            [(> 1 (length nl))
             (error (format "Duplicate ~a found" norm-alt-name))]
            [else (car nl)]))
    (filter-map
     (lambda (br)
       (if (string-prefix? alt-name-pref (br-name br))
           (let ([new-name (substring (br-name br) (string-length alt-name-pref))]
                 [opts (benchmark-result-opts br)]
                 [trial-stats (benchmark-result-trial-stats br)])
             (normalize-br
              (norm-br new-name)
              (bresult
               (struct-copy benchmark-opts opts [name new-name])
               trial-stats)))
           #f))
     brs))
  (define num-alts (length alt-names))
  (define alt-nums (for/list ([i num-alts]) i))
  (define colors (color-seq "red" "green" num-alts))
  (define start-xs (linear-seq 0 (- num-alts 1) num-alts))
  (define skip (+ num-alts 1))
  (define bar-width (- (/ skip (+ num-alts 1)) (discrete-histogram-gap)))
  (append-map
   (lambda (an c x)
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
      (error-bars (list (vector
                         (+ start-x (* skip i) delta-to-mid-bar)
                         conf-mean
                         conf-ht))
                          #:line-width 1)))
  (cons   
   (discrete-histogram (map data-point brs)
                       #:skip skip
                       #:label alt-name                      
                       #:color color
                       #:x-min start-x)
   (map data-error-bars brs (for/list ([i (length brs)]) i))))

