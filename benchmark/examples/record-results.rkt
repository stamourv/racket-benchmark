#lang racket

(require benchmark rackunit)

(define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))
(define fib-group
  (parameterize ([num-trials 30]
                 [gc-between #f])
    (make-bench-group
     "fibs"
     (list (bench-one "fib 18" (fib 18))
           (bench-one "fib 19" (fib 19))))))

(define results-list
  (map (lambda (i) (run-benchmarks fib-group)) (list 0 1 2)))

(define file-1 "fake-record-results")
(define file-2 "fake-record-RESULTS")

;; file (index into results-list) version -> void?
(define (test-record-get f i v)
  (check-record-results f i v)
  (check-get-results f i v))

;; file (index into results-list) version -> void?
(define (check-record-results f i v)
  (define out (open-output-bytes))
  (parameterize ([current-output-port out])
    (record-results (list-ref results-list i) f)
    (check-equal? (bytes->string/utf-8 (get-output-bytes out))
                  (format "Wrote results to ~a-~a\n" f v))))

;; file (index into results-list) version -> void?
(define (check-get-results f i [v #f])
  (check-equal? (get-past-results f v) (list-ref results-list i)))

;; remove all existing results for this test
(system (format "rm -f ~a-* ~a-*" file-1 file-2))

;; record results 0 in file-1 @ version 0
(test-record-get file-1 0 0)
;; record results 0 in file-1 @ version 1
(test-record-get file-1 0 1)
;; record results 0 in file-1 @ version 2
(test-record-get file-1 0 2)

;; remove file-1 @ version 1
(system "rm -f fake-record-results-1")

;; record results 1 in file-1 @ version 1
(test-record-get file-1 1 1)
;; record results 1 in file-1 @ version 3
(test-record-get file-1 1 3)

;; record results 2 in file-2 @ version 0
(test-record-get file-2 2 0)
;; record results 2 in file-2 @ version 1
(test-record-get file-2 2 1)

;; check all recorded results, specifying version
(check-get-results file-1 0 0)
(check-get-results file-1 1 1)
(check-get-results file-1 0 2)
(check-get-results file-1 1 3)
(check-get-results file-2 2 0)
(check-get-results file-2 2 1)

;; check latest results, not specifying version
(check-get-results file-1 1)
(check-get-results file-2 2)

;; remove all existing results for this test
(system (format "rm -f ~a-* ~a-*" file-1 file-2))
