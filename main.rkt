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
  (filter (curry regexp-match #rx"\\.c$") args))

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
      (define tmp-xml (build-path subdir "__tmp.xml"))
      (define tmp-c (build-path subdir "__dst.c"))
      (define out-c (build-path subdir filename))

      (when DEBUG_MODE
        (eprintf "[ACCSAT] ~a compiled in ~a\n" s subdir))

      ;; Run the preprocessor; Convert to XML
      (define e (process-output->string
                 ;; -D__CUDACC__ disables float128
                 `(,cc "-E -D__CUDACC__" ,@macros ,@accopt ,@incdir ,s
                       "| C_Front -facc -fopenmp --no-builtin-va-arg"
                       "| grep -av '<linemarker lineno=' > "
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

      ;; Convert back to C
      (process-output->string
       `("java -Xss4m -cp"
         ,(format
           "~a/om-c-back.jar:~a/om-common.jar:~a/om-exc-tools.jar"
           jar-dir jar-dir jar-dir)
         "exc.util.omompx -decomp -xc"
         ,(path->string dst-xml) "-o" ,(path->string tmp-xml)))

      ;; Adjust output
      (adjust-output out-c tmp-c)

      (when DEBUG_MODE
        (eprintf "[ACCSAT] Saturated ~a saved as ~a.sat\n" s s)
        (system (~a "cp " out-c " " s ".sat")))

      (path->string out-c)))

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
