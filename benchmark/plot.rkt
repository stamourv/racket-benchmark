#lang racket

(require "types.rkt" "util.rkt" "stats.rkt")
(require plot plot/utils srfi/13)

(provide render-benchmark-alts
         current-benchmark-color-scheme
         bright-color-scheme
         pastel-color-scheme
         black-white-color-scheme-short
         black-white-color-scheme-medium-1
         black-white-color-scheme-medium-2
         black-white-color-scheme-long
         benchmark-show-legend?)

;; TODO document those
(define bright-color-scheme
  (cons '("red" "blue" "dark green" "white" "purple" "black" "yellow"
          "dark gray")
        '(solid)))
(define pastel-color-scheme
  (cons '("Salmon" "DarkBlue" "Yellow Green" "Black" "MediumOrchid" "white"
          "Gold" "dark gray")
        '(solid)))

;; TODO automatically pick between those depending on n of bars
(define black-white-color-scheme-short
  (cons '("white" "Dark Gray" "black")
        '(solid)))
(define black-white-color-scheme-medium-1
  (cons '("Dark Gray" "white" "gray" "black" "Light Gray")
        '(solid)))
(define black-white-color-scheme-medium-2
  (cons '("black" "white" "black" "black" "black")
        '(bdiagonal-hatch solid horizontal-hatch solid fdiagonal-hatch)))
(define black-white-color-scheme-long
  (cons '("Dim Gray" "black" "white" "black" "black" "black" "Dark Gray"
          "black")
        '(solid bdiagonal-hatch solid horizontal-hatch solid
          fdiagonal-hatch solid vertical-hatch)))

(define current-benchmark-color-scheme (make-parameter pastel-color-scheme))

(define benchmark-show-legend? (make-parameter #t))


;; render-benchmark-alts : (listof string?) (listof benchmark-result?)
;;                         string? -> renderer2d?
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
           (let ([new-name
                  (substring (br-name br) (string-length alt-name-pref))]
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
  (define alt-nums (for/list ([i (in-range 0 num-alts)]) i))
  (define colors
    (for/list ([i (in-range num-alts)]
               [c (in-cycle (car (current-benchmark-color-scheme)))])
      c))
  (define styles
    (for/list ([i (in-range num-alts)]
               [c (in-cycle (cdr (current-benchmark-color-scheme)))])
      c))
  (define start-xs (linear-seq 0 (- num-alts 1) num-alts))
  (define skip (+ num-alts 1))
  (define bar-width (- (/ skip (+ num-alts 1)) (discrete-histogram-gap)))
  (append-map
   (lambda (an c s x)
     (render-benchmark-alt an (select-benchmarks an) c s x skip bar-width))
   alt-names
   colors
   styles
   start-xs))

;; render-benchmark-alt : string? benchmark-result? plot-color/c
;;                        plot-brush-style/c rational? (>=/c 0) (>/c 0)
;;                        -> (listof renderer2d?)
(define (render-benchmark-alt alt-name brs color style start-x skip bar-width
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
   (if (benchmark-show-legend?)
       (discrete-histogram (map data-point brs)
                       #:skip skip
                       #:label alt-name
                       #:color color
                       #:style style
                       #:x-min start-x)
       (discrete-histogram (map data-point brs)
                       #:skip skip
                       ;; no labels, that disables the legend
                       #:color color
                       #:style style
                       #:x-min start-x))
   
   (map data-error-bars brs (for/list ([i (in-range 0 (length brs))]) i))))
