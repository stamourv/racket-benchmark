#lang racket

(require benchmark "sleep.rkt")

(plot-results (run racket-time-extract-result) "sleep-racket-time.pdf")

;; Local Variables:
;; compile-command: "racket sleep-racket-time.rkt"
;; End:
