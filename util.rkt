#lang racket

(require
 sxml
 srfi/1)

(provide (all-defined-out))

(define ~ list-ref)

(define (acons key val alist)
  (cons (cons key val) alist))

(define (assoc-adjoin alist key val)
  (define index (list-index (lambda (x) (equal? (car x) key)) alist))
  (if (not index) (acons key val alist)
      (let ()
        (define-values (l r) (split-at alist index))
        (append l (acons key val (cdr r))))))

(define (assoc-ref alist key [default #f])
  (define x (assoc key alist))
  (if x (cdr x) default))

;; alist ::= '((key . val) ...)  ->  '((val key ...) ...)
(define (reverse-assoc alist)
  (map
   (lambda (x) (cons (cdar x) (map car x)))
   (group-by cdr alist)))

;; alist ::= '((key val ...) ...) - > '((val key ...) ...)
(define (reverse-assoc-multi alist)
  (define val-to-key
    (append-map (lambda (x) (map (curryr cons (car x)) (cdr x))) alist))
  (map
   (lambda (x) (cons (caar x) (map cdr x)))
   (group-by car val-to-key)))

(define (sxml:snip x)
  `(,(sxml:name x) ,@(sxml:content-raw x)))

(define (make-sxpath-query pred?)
  (lambda (nodeset . rest)
    ((sxml:filter pred?) nodeset)))

(define (sxpath:name name)
  (make-sxpath-query (ntype?? name)))

(define-syntax and-let*-decl
  (syntax-rules ()
    [(_ (var expr))
     (define var expr)]

    [(_ (expr)) #f]

    [(_ var) #f]))

(define-syntax and-let*-cond
  (syntax-rules ()
    [(_ (var expr)) var]

    [(_ (expr)) expr]

    [(_ var) var]))

(define-syntax and-let*
  (syntax-rules ()
    [(_ () body ...)
     (let () body ...)]

    [(_ (x) body ...)
     (let ()
       (and-let*-decl x)
       (if (and-let*-cond x) (let () body ...) #f))]

    [(_ (x y ...) body ...)
     (and-let* (x) (and-let* (y ...) body ...))]))

(define-syntax and-let1
  (syntax-rules ()
    [(_ var val body ...)
     (and-let* ([var val]) body ...)]))

(define-syntax let1
  (syntax-rules ()
    [(_ var expr body ...)
     (let ((var expr)) body ...)]))

(define-syntax rlet1
  (syntax-rules ()
    [(_ var expr body ...)
     (let1 var expr body ... var)]))

(define-syntax if-let1
  (syntax-rules ()
    [(_ var expr then else)
     (let1 var expr
           (if var then else))]

    [(_ var expr then)
     (let1 var expr
           (when var then))]))

(module+ test
  (require rackunit rackunit/text-ui)
  (define-syntax ==
    (syntax-rules ()
      [(_ val ...) (check-equal? val ...)]))

  (run-tests
   (test-suite "util"
    (test-case ""
      (== #f #f)
      ))))
