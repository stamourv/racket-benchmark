#lang racket

(provide get-past-results
         record-results)

;; path? exact-integer? -> (listof benchmark-result?)
(define (get-past-results file [version #f])
  (file->value (get-file file version) #:mode 'text))

;; (listof benchmark-result?) path? -> void?
(define (record-results results file)
  (let ([fresh-name (mk-fresh-file-name file)])
    (write-to-file results fresh-name #:mode 'text #:exists 'truncate)
    (displayln (format "Wrote results to ~a" fresh-name))))

;; string? (or/c exact-integer? #f) -> string?
;; get latest version of file-base if version #f
;; if version not #f, get (fmt file-base version)
(define (get-file file-base version)
  (define (get-file-names [v 0])
    (let ([file-name (fmt file-base v)])
      (if (file-exists? file-name)
          (cons file-name (get-file-names (+ 1 v)))
          (list))))
  (let ([file-names (get-file-names)])
    (cond
     ;; no specific version and file-names not null
     [(and (not version) (not (null? file-names)))
      (last file-names)]
     ;; specific version and assoc. file exists
     [(and version (file-exists? (fmt file-base version)))
      (fmt file-base version)]
     ;; specific version and assoc. file doesn't exist
     [version
      (error 'get-file "No file found: ~a" (fmt file-base version))]
     ;; no specific version, but no matching files exist
     [else
      (error 'get-file "No files found matching ~a-([0-9]+)"
              (fmt file-base version))])))

;; string? [exact-integer?] -> string?
;; create a new fresh file name of the form file-base-<n>
(define (mk-fresh-file-name file-base [version 0])
  (if (file-exists? (fmt file-base version))
      (mk-fresh-file-name file-base (+ version 1))
      (fmt file-base version)))

;; string? exact-integer? -> string?
;; format a file name given the file-base and version
(define (fmt file-base version)
  (format "~a-~a" file-base version))
