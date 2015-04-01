#lang racket

(require "types.rkt" "bootstrap-ci.rkt")
(require plot/no-gui plot/utils math/statistics)

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
  (cons '("red" "blue" "dark green" "Light Gray" "purple" "DimGray" "yellow")
        '(solid)))
(define pastel-color-scheme
  (cons '("Salmon" "DarkBlue" "Yellow Green" "DimGray" "MediumOrchid"
          "Light Gray" "Gold")
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

(struct bootstrapped-ci (name opts mean conf-lb conf-ub))

;; render-benchmark-alts : (listof any/c) (listof benchmark-result?)
;;                         -> renderer2d?
(define (render-benchmark-alts
         norm-opts
         brs
         #:format-opts
         [format-opts (lambda (opts) (apply ~a opts #:separator " "))]
         #:normalize?
         [normalize #t])
  (define names (remove-duplicates (map benchmark-result-name brs)))
  (define opts (remove-duplicates (map benchmark-result-opts brs)))
  (define (norm-br name)
    (define filtered-brs
      (filter (lambda (br)
                (and (equal? name (benchmark-result-name br))
                     (equal? norm-opts (benchmark-result-opts br))))
              brs))
    (if (null? filtered-brs)
        (error (format
                "Could not find standard benchmark: name ~a, opts ~a"
                name
                norm-opts))
        (car filtered-brs)))
  (define norm-brs
    (make-immutable-hash
     (map (lambda (n) (cons n (norm-br n))) names)))
  (define (speedup-bootstrapped-ci norm-br br)
    ;; (define br/norm-br
    ;;   (map /
    ;;        (benchmark-result-trial-times br)
    ;;        (benchmark-result-trial-times norm-br)))
    ;; TODO bootstrapping + confidence intervals doesn't work for baseline:
    ;;   we end up computing confidence interval of the ratio X/X, which has
    ;;   a clear dependency between num and den, so doesn't work
    ;;   Neil ⊥ suggests shuffling X's samples, to remove dependency, but that
    ;;   doesn't work.
    ;;   for now, using 1 standard dev on each side for error bars.
    ;; TODO investigate
    ;; (define bootstrapped-sample
    ;;   (nonparametric2d-bootstrap
    ;;    mean-quotient
    ;;    (benchmark-result-trial-times br)
    ;;    (benchmark-result-trial-times norm-br)))
    (define name (benchmark-result-name br))
    (define opts (benchmark-result-opts br))
    (define mean-br ; scaling down mean by mean of baseline
      (if normalize
          (with-handlers ([exn:fail:contract:divide-by-zero? (λ (e) 0)])
            (/ (mean (benchmark-result-trial-times br))
               (mean (benchmark-result-trial-times norm-br))))
          (mean (benchmark-result-trial-times br))))
    (define stddev-br ; scaling down stddev by mean of baseline
      (if normalize
          (with-handlers ([exn:fail:contract:divide-by-zero? (λ (e) 0)])
            (/ (stddev (benchmark-result-trial-times br))
               (mean (benchmark-result-trial-times norm-br))))
          (stddev (benchmark-result-trial-times br))))
    ;; New attempt at confidence intervals, using a straight formula instead of
    ;; Neil's stuff.
    (define margin-of-error
      (* 1.96 ; Z value for a 95% confidence interval
         (/ stddev-br
            (sqrt (length (benchmark-result-trial-times br))))))
    (log-message benchmark-logger 'info
                 (~a name " " opts
                     "; normalized mean = " (exact->inexact mean-br)
                     "; normalized stddev = " stddev-br)
                 #f)
    (bootstrapped-ci
     name
     opts
     ;; TODO: is this the proper way to calculate mean?
     mean-br
     ;; TODO not confidence intervals anymore, see above
     ;; (bootstrap-bca-conf mean br/norm-br bootstrapped-sample .025)
     ;; (bootstrap-bca-conf mean br/norm-br bootstrapped-sample .975)
     ;; TODO just stddevs, not standard
     ;; (+ mean-br stddev-br)
     ;; (- mean-br stddev-br)
     (+ mean-br margin-of-error)
     (- mean-br margin-of-error)))
  (define normalized-benchmarks
    (map
     (lambda (br)
       (speedup-bootstrapped-ci
        (hash-ref norm-brs (benchmark-result-name br))
        br))
     brs))
  (define (select-benchmarks opts)
    (filter (lambda (br) (equal? opts (bootstrapped-ci-opts br)))
            normalized-benchmarks))
  (define num-alts (length opts))
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
   (lambda (o c s x)
     (render-benchmark-alt
      (format-opts o)
      (select-benchmarks o)
      c
      s
      x
      skip
      bar-width))
   opts
   colors
   styles
   start-xs))

;; render-benchmark-alt : string? benchmark-result? plot-color/c
;;                        plot-brush-style/c rational? (>=/c 0) (>/c 0)
;;                        -> (listof renderer2d?)
(define (render-benchmark-alt alt-name bcis color style start-x skip bar-width)
  (define (data-point bci)
    (vector (bootstrapped-ci-name bci) (bootstrapped-ci-mean bci)))
  (define (data-error-bars bci i)
    (let* ([conf-lb (bootstrapped-ci-conf-lb bci)]
           [conf-ub (bootstrapped-ci-conf-ub bci)]
           [conf-mean (bootstrapped-ci-mean bci)]
           [conf-ht (- conf-ub conf-mean)]
           [delta-to-mid-bar (/ (+ bar-width (discrete-histogram-gap)) 2)])
      (error-bars (list (vector
                         (+ start-x (* skip i) delta-to-mid-bar)
                         conf-mean
                         conf-ht)))))
  (cons
   (if (benchmark-show-legend?)
       (discrete-histogram (map data-point bcis)
                       #:skip skip
                       #:label alt-name
                       #:color color
                       #:style style
                       #:x-min start-x)
       (discrete-histogram (map data-point bcis)
                       #:skip skip
                       ;; no labels, that disables the legend
                       #:color color
                       #:style style
                       #:x-min start-x))
   (map data-error-bars bcis (for/list ([i (in-range 0 (length bcis))]) i))))

(define (mean-quotient ys xs)
  (mean (map / ys xs)))
