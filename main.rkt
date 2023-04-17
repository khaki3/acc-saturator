#lang racket

(require
 "util.rkt"
 "adjust.rkt"
 "sat.rkt"
 srfi/1
 sxml
 racket/os)

(define DEBUG_MODE (getenv "ACCSAT_DEBUG"))

(define (extract-source-files args)
  (filter (curry regexp-match #rx"\\.[fF]90$") args))

(define (extract-include-directories args)
  (filter (curry regexp-match #rx"^-I") args))

(define (extract-macros args)
  (filter (curry regexp-match #rx"^-D") args))

(define (extract-acc-options args)
  (filter
   (disjoin
    (curry equal? "-fopenacc")
    (curry equal? "-acc"))
   args))

(define (replace-args args from to)
  (define alist (map cons from to))
  (for/list ([a args])
    (or (assoc-ref alist a) a)))

(define (process-output->string command)
  (with-output-to-string
    (lambda () (system (string-join command)))))

;; make-temporary-directory is not defined in some environments
(define (%make-temporary-directory)
  (define temp (make-temporary-file (~a "accsat~A" (getpid))))
  (delete-file temp)
  (make-directory* temp)
  temp)

;; (define (%make-temporary-file dir template)
;;   (define fullpath (string-append (path->string dir) "/" template))
;;   (string->path (make-temporary-file fullpath)))

(define (detect-ompjar-dir)
  (define bin (find-executable-path "ompcc"))
  (define usr (drop-right (explode-path bin) 2))
  (apply build-path `(,@usr "share")))

(define (main args)
  (define source (extract-source-files args))
  (define incdir (extract-include-directories args))
  (define accopt (extract-acc-options args))
  (define macros (extract-macros args))
  (define cc (and (>= (length args) 2) (car args)))
  (define dir (%make-temporary-directory))
  (define jar-dir (path->string (detect-ompjar-dir)))

  (define new-source
    (for/list ([s source] [i (iota (length source))])
      (define subdir (build-path dir (number->string i)))
      (define filename (last (explode-path s)))
      (make-directory subdir)

      (define src-xml (build-path subdir "__src.xml"))
      (define dst-xml (build-path subdir "__dst.xml"))
      (define out-f (build-path subdir filename))

      (when DEBUG_MODE
        (eprintf "[ACCSAT] ~a compiled in ~a\n" s subdir))

      ;; Run the preprocessor; Convert to XML
      (define e (process-output->string
                 `(,cc "-cpp -E" ,@macros ,@accopt ,@incdir ,s
                       "| F_Front -facc > "
                       ,(path->string src-xml))))

      ;; Load as SXML
      (define in-sxml
        (call-with-input-file src-xml
          (curryr ssax:xml->sxml '())))

      ;; Run equality saturation
      (define out-sxml (sat in-sxml))

      ;; Output XML
      (call-with-output-file dst-xml
        (curry srl:sxml->xml out-sxml))

      ;; Convert back to F
      (process-output->string
       `("F_Back" ,(path->string dst-xml) "-o" ,(path->string out-f)))

      (when DEBUG_MODE
        (eprintf "[ACCSAT] Saturated ~a saved as ~a.sat\n" s s)
        (system (~a "cp " out-f " " s ".sat")))

      (path->string out-f)))

  ;; Execute cc while replacing sources
  (process-output->string (replace-args args source new-source))

  ;; Clean temporary files
  (when (not DEBUG_MODE)
    (for ([d (directory-list dir #:build? #t)])
      (for-each delete-file (directory-list d #:build? #t))
      (delete-directory d))
    (delete-directory dir)))

(module+ main
  ;; Define a variable to omit the output of the return value
  (define exit-code (main (vector->list (current-command-line-arguments)))))

(module+ test
  (require rackunit rackunit/text-ui)

  (run-tests
   (test-suite "main"
    (test-case ""
      (check-equal? #f #f))
    )))
