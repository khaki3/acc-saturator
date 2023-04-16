#lang racket

(require
 "util.rkt"
 srfi/1
 srfi/13)

(provide (all-defined-out))

#|

ACC ADJUSTOR

1) Normalize loops
---
#pragma acc parallel
{
    int i;
    for (i = 0; ...
---

 ||
 \/

---
#pragma acc parallel
for (int i = 0; ...
---

2) Normalize pointer-ref
---
*(p + ...)
---

  ||
  \/

---
p[...]
---

|#

;; return (var . type) or #f
(define (get-declaration line)
  (and-let1 ma (regexp-match #px"^(\\w+) (\\w+);$" line)
    (and ma (cons (~ ma 2) (~ ma 1)))))

(define (for? line)
  (regexp-match #rx"^for" line))

(define (get-for-var line)
  (and-let1 ma (regexp-match #px"^for\\((\\w+) (.*)$" line)
    (~ ma 1)))

(define (for-insert-declaration line env)
  (or
   (and-let1 ma (regexp-match #px"^for\\((\\w+) (.*)$" line)
     (let1 var (~ ma 1)
       (if-let1 type (assoc-ref env var)
         (string-append "for(" type " " var (~ ma 2))
         line)))
   line))

;; drop the first brace pair ('{' and '}')
(define (drop-brace-pair states)
  (and-let1 rest (and (string=? (car states) "{") (cdr states))
    (let loop ([braces 1] [rest rest] [ret '()] [state-num 0])
        (cond [(not (= braces 0))
               (let* ([head (car rest)]
                      [b (+ braces (count-braces head))])
                 (loop b (cdr rest) (cons head ret)
                       (+ state-num
                          [cond
                            ;; end of {}
                            [(and (not (= braces b)) (= b 1)) 1]

                            ;; var decl
                            [(get-declaration head) 0]

                            ;; \;
                            [(and (= braces b) (= b 1)
                                  (regexp-match
                                   #rx";"
                                   (regexp-replace #rx"(#|//).*$" head "")))
                             1]

                            [else 0]])))]

              [(not (= state-num 1)) states]

              [else (append (reverse (cdr ret)) rest)]))))

(define (adjust-acc-for states)
  (define for-vars (filter-map get-for-var states))

  ;; env := '((var . type) ...)
  (let loop ([ret '()] [states states] [env '()] [under-dir #f])
    (if (null? states) (reverse ret)

        (let ([line (car states)] [rest (cdr states)])
          (cond [(regexp-match #rx"^#pragma acc (kernels|parallel|loop)" line)

                 (loop (cons line ret)
                       (if (string=? (car rest) "{")
                           (drop-brace-pair rest) rest)
                       env #t)]

                [(and-let1 d (get-declaration line)
                   (and (member (car d) for-vars) d))
                 => (lambda (bind) (loop ret rest (cons bind env) under-dir))]

                [(for? line)
                 (loop (cons (for-insert-declaration line env) ret)
                       rest env #f)]

                [else
                 (loop (cons line ret) rest env under-dir)]
                )))))

(define (count-braces str)
  (rlet1 ret 0
    (string-for-each
     (lambda (c)
       (cond [(char=? c #\{) (set! ret (+ ret 1))]
             [(char=? c #\}) (set! ret (- ret 1))]))
     str)))

(define (replace-pointer-ref str)
  (or
   (and-let* ([ma
               (regexp-match
                #rx"\\*\\(\\(([^\\)]*)\\)\\(&\\((.*)\\)\\)\\" str)]
              [expr (adjust-pointer-ref (~ ma 2))])
     ;; *((type)(&(expr))) -> *((type)(&(@expr)))   ; no change except expr
     (format "*((~a)(&(~a)))" (~ ma 1) expr))

   (and-let* ([ma
               (regexp-match
                #rx"^\\*\\(\\(([^\\)]*)\\)\\((.*)\\)\\)$" str)]
              [expr (adjust-pointer-ref (~ ma 2))])
     ;; *((type)(expr)) -> *((type)(@expr))
     (format "*((~a)(~a))" (~ ma 1) expr))

   (and-let* ([ma
               (regexp-match
                #px"^\\*\\((\\w+) \\+ (.*)\\)$" str)]
              [index (adjust-pointer-ref (~ ma 2))])
     ;; *(var + index) -> var[@index]
     (format "~a[~a]" (~ ma 1) index))

   (and-let* ([ma
               (regexp-match
                #rx"^\\*\\(\\((.*)\\) \\+ (.*)\\)$" str)]
              [index (adjust-pointer-ref (~ ma 2))])
     ;; *((expr) + index) -> #expr[@index]
     (format "~a[~a]" (replace-pointer-ref (~ ma 1)) index))

   str))

(define (adjust-pointer-ref str)
  (let loop ([ret '()] [stack '()] [lst (string->list str)] [lparams 0])
    (let1 inner (pair? stack)
      (cond [(and inner (= lparams 0))
             (loop (append
                    (reverse
                     (string->list
                      (replace-pointer-ref
                       (list->string
                        (reverse stack)))))
                    ret)
                   '() lst 0)]

            [(null? lst)
             (list->string (reverse (append stack ret)))]

            [else
             (let* ([head (car lst)] [rest (cdr lst)]
                    [lp (char=? #\( head)]
                    [rp (char=? #\) head)])

               (cond [(and lp inner)
                      (loop ret (cons #\( stack) rest (+ lparams 1))]

                     [(and lp (and (pair? ret) (char=? (car ret) #\*)))
                      (loop (cdr ret) (list #\( #\*) rest 1)]

                     [(and rp inner)
                      (loop ret (cons #\) stack) rest (- lparams 1))]

                     [inner
                      (loop ret (cons head stack) rest lparams)]

                     [else
                      (loop (cons head ret) '() rest 0)]))]
            ))))

(define (adjust-main)
  (let loop0 ([line (read-line)])

    (cond [(eof-object? line) #f]

          [(regexp-match #rx"^#pragma acc (kernels|parallel)" line)

           (let loop ([states (list line)] [braces (count-braces line)])

             (let1 line (read-line)

               (if (regexp-match #rx"^#" line)
                   (loop (cons line states) braces)

                   (let ([states (cons line states)]
                         [braces (+ braces (count-braces line))])

                     (if (= braces 0)
                         (for-each (compose displayln adjust-pointer-ref)
                                   (adjust-acc-for (reverse states)))
 
                         (loop states braces))))))]

          [else (displayln (adjust-pointer-ref line))])

    (unless (eof-object? line)
      (loop0 (read-line)))))

(define (adjust-output dst src)
  (with-output-to-file dst
    (thunk (with-input-from-file src adjust-main))))
