#lang racket

(require racket/date racket/serialize "types.rkt")

(provide get-past-results
         record-results
         make-fresh-file-name) ; for incremental result recording

;; path? exact-integer? -> bench-results?
(define (get-past-results file [version #f])
  (map deserialize (file->list (get-file file version) #:mode 'text)))

;; bench-results? path? -> void?
(define (record-results results file)
  (let ([fresh-name (make-fresh-file-name file)])
    ;; serialization is used as otherwise read (write date) /= date
    ;; in the equal? sense
    (with-output-to-file
        fresh-name #:mode 'text #:exists 'truncate
      ;; write each result separately, as opposed to a single big list,
      ;; to allow incremental recording
      (lambda ()
        (for ([r (in-list results)])
          (write (serialize r)))))
    (displayln (format "Wrote results to ~a" fresh-name))))

;; get latest version of file-base if version #f
;; if version not #f, get (fmt file-base version)
(define (get-file file-base version)
  (define-values (dir file _) (split-path file-base))
  (define file-candidates
    (for*/list ([f (in-directory (if (eq? dir 'relative)
                                     (current-directory)
                                     dir))]
                [n (in-value (regexp-match (format "^(.+/)?~a-([0-9]+)"
                                                   (path->string file))
                                           (path->string f)))]
                #:when n)
      (cons f (string->number (third n)))))
  (define sorted-file-candidates
    (map car (sort file-candidates > #:key cdr)))
  (cond
   ;; no specific version and sorted-file-candidates not null
   [(and (not version) (not (null? sorted-file-candidates)))
    (first sorted-file-candidates)]
   ;; specific version and assoc. file exists
   [(and version (file-exists? (fmt file-base version)))
    (fmt file-base version)]
   ;; specific version and assoc. file doesn't exist
   [version
    (error 'get-file "No file found: ~a" (fmt file-base version))]
   ;; no specific version, but no matching files exist
   [else
    (error 'get-file "No files found matching ~a-([0-9]+)"
           file-base)]))

;; string? [exact-integer?] -> string?
;; create a new fresh file name of the form file-base-<n>
(define (make-fresh-file-name file-base [version 0])
  (if (file-exists? (fmt file-base version))
      (make-fresh-file-name file-base (+ version 1))
      (fmt file-base version)))

;; string? exact-integer? -> string?
;; format a file name given the file-base and version
(define (fmt file-base version)
  (format "~a-~a" file-base version))
