#lang racket

(require
 "util.rkt"
 sxml
 srfi/1
 srfi/13)

(provide (all-defined-out))

(define DEBUG_MODE (getenv "ACCSAT_DEBUG"))
(define type-reduct #f)
(define new-type-table '())

(define (sat sxml)
  (set! type-reduct (make-type-reduction-fn sxml))
  ((compose
    (update 'FfunctionDefinition sat-top)
    (update 'typeTable update-type-table))
   sxml))

(define (update-type-table tb)
  (define content (sxml:content tb))
  (sxml:change-content tb (append content new-type-table)))

(define (make-type-reduction-fn sxml)
  (define mapping
    (map
     (lambda (bt)
       (define type (sxml:attr bt 'type))
       (define ref (sxml:attr bt 'ref))
       (if (not (member ref '("Fcomplex" "Freal" "Fint")))
           (cons type ref)

           (let ()
             (define new-type (~a type "__noptr"))
             (define new-bt
               (sxml:change-attrlist bt `((type ,new-type) (ref ,ref))))
             (set! new-type-table (cons new-bt new-type-table))
             (cons type new-type))))
     ((sxpath "//FbasicType") sxml)))
  (set! new-type-table (reverse new-type-table))
  (lambda (type)
    (let loop ([t type])
      (define ref (assoc-ref mapping t))
      (if ref (loop ref) t))))

(define (sat-top sxml)
  (define (i? x)
    (define name ((if-car-sxpath "name/text()") x))
    (string-prefix? "t__i" name))
  (define tmp (%sat sxml))
  (define new-sym (filter i? ((sxpath "//blockStatement/symbols/id") tmp)))
  (define new-dec (filter i? ((sxpath "//blockStatement/declarations/varDecl") tmp)))
  ((compose
    remove-block
    (update 'symbols
            (lambda (n)
              (define content (sxml:content n))
              (sxml:change-content n (append content new-sym))))
    (update 'declarations
            (lambda (n)
              (define content (sxml:content n))
              (sxml:change-content n (append content new-dec))))
    (update 'FfunctionDefinition sat-top))
   tmp))

(define (%sat sxml)
  ((compose (update 'body omp-updater)
            (update 'ACCPragma acc-updater)) sxml))

(define (update symbol proc)
  (lambda (node)
    (if (not (pair? node)) node
         (cons
          (car node)
          (cdr 
           (pre-post-order
            ;; Skip the top node
            (cons '_tmp (cdr node))
            `((,symbol *preorder* . ,(lambda args (proc args)))
              (*text* . ,(lambda (tag content) content))
              (*default* . ,(lambda args args)))))
          ))))

(define (acc-updater node)
  (define dirname ((if-car-sxpath "string") node))

  (define fn
    (match dirname
      [(list _ (or "PARALLEL" "PARALLEL_LOOP"))
       parallel-updater]

      [(list _ (or "KERNELS" "KERNELS_LOOP"))
       kernels-updater]

      [else %sat]))

  (fn node))

(define (remove-block node)
  (define (proc node)
    `(FifStatement
      (condition (FlogicalConstant ".TRUE."))
      (then ,(remove-block ((car-sxpath "body") node)))))
  ((update 'blockStatement proc) node))

(define (parallel-updater node)
  ((update 'FdoStatement for-updater) node))

(define kernel-count 0)

(define (for-updater node)
  (set! kernel-count (+ kernel-count 1))
  (define tmp (%for-updater node))
  ((compose
    (update 'name (curryr rewrite-tmpvar kernel-count))
    (update 'Var (curryr rewrite-tmpvar kernel-count))) tmp))

(define (rewrite-tmpvar node count)
  (define t (sxml:text node))
  (if (tmpvar? t) `(,(sxml:name node) ,(~a "t__i" count t)) node))

(define (kernels-updater node)
  (parallel-updater node))

(define (omp-updater node)
  (let loop ([content (sxml:content node)] [kernel #f] [mod '()])
    (cond [(null? content)
           (define vars
             (filter
              (curry string-prefix? "t__i")
              ((sxpath "//blockStatement/symbols/id/name/text()") (cons '_head mod))))
           ((compose
             (update 'FpragmaStatement (curryr attach-private vars))
             (update 'body omp-updater))
            (sxml:change-content node (reverse mod)))]

          [(and (eq? 'FdoStatement (sxml:name (car content))) kernel)
           (loop (cdr content) #f
                 (cons (cadr (parallel-updater (list '_head (car content)))) mod))]

          [(and (eq? 'FpragmaStatement (sxml:name (car content)))
                (regexp-match #rx"(?i:teams)" (sxml:text (car content))))
           (loop (cdr content) #t (cons (car content) mod))]

          [else
           (loop (cdr content) kernel (cons (car content) mod))])))


;;
;; 1. Get kernel code
;;    { S1 = ..; S2 = ..; ... }
;;
;; 2. Make SSA without if/for/while/.. structures
;;    (( S1 .. ) ( S2 .. ) ...)
;;
;; 3. Put each expression in a e-graph in Rust
;;
;; 4. Saturate; Extract with ILP to select all the expression
;;    (Get the minimum of (list s0 s1 s2 ...) )
;;
;; 5. Update SSA with extended statements for common sub-expressions
;;    (( E1 .. ) ( S1 ..) ( S2 ..) ...)
;;
;; 6. Update kernel code while checking corresponding statements by hashing
;;    { E1 = ..; S1 = ..; S2 = ..; ... }
;;

;;
;; SSA conversion
;;
;; e.g. a[i] = x; a[i+1] = a[i]+1;
;;      => (( a0 (st a a i x) ) (a1 (st a a0 (+ i 1) (+ (ld a0 i) 1))))
;;
;;      for (i) { a[i] = a[i-1]; a[i*2] = a[i-1]; }
;;      => (( a0 (st a (phi for-cond for-a a-init) i
;;                   (ld a (phi for-cond for-a a-init) (- i 1)) ))
;;          ( a1 (st a a0 (* i 2) (ld a0 (- i 1)) )))
;;
;;      if (k) { a += 1 } a += 1;
;;      => (( a0 (+ a-init 1)) (a1 (+ (phi k a0 a-init) 1)))
;;

[define (%for-updater node)
  (define-values (ssa0 _) (extract-ssa node))
  (define ssa1 (reduce-st-chain ssa0))
  (define ssa2 (reverse (delete-unnecessary-phi ssa1)))

  ;; var + hash concat
  (define sat-in
    (map (match-lambda [(list var hash expr)
                        (list (format "~a__~a" var hash) expr)])
         ssa2))

  (define rewriter-path
    (build-path
     (path-only (path->complete-path (find-system-path 'run-file)))
     "rewriter/target/release/rewriter"))

  (define output-path (make-temporary-file))

  (define-values (sp oport iport eport)
    (subprocess #f #f #f rewriter-path output-path))

  (displayln sat-in iport)
  (close-output-port iport)
  (subprocess-wait sp)

  (if DEBUG_MODE
      (for-each (curry eprintf "[ACCSAT] ~a \n") (port->lines eport))
      (close-input-port eport))

  (close-input-port oport)
  (define tmp-out
    (let ()
      (define x (file->list output-path))
      (if (null? x) '()
          (car x))))

  (define sat-out
    ;; symbol->string map except op
    (let ()
      (define (each x)
        (list (f (~ x 0)) (f (~ x 1))))
      (define (f x)
        (cond [(pair? x) (cons (car x) (map f (cdr x)))]
              [(symbol? x) (symbol->string x)]
              [else x]))
      (map each tmp-out)))

  (define out-ssa
    (map (match-lambda [(list id expr)
                        (define x (string-split id "__"))
                        (define v (~ x 0))
                        (define h (if (tmpvar? v) (~ x 1)
                                      (string->number (~ x 1))))
                        `(,v ,h ,expr)])
         sat-out))

  (update-with-ssa node out-ssa)]

(define (update-with-ssa node env)
  ;; Put extensional variables and new expressions
  (extend-node node env))

(define TMPTAG "_v")

(define (tmpvar? v)
  ;; string-prefix? in srfi-13's order
  (string-prefix? TMPTAG v))

(define (remove-varhash var)
  (car (string-split var "__")))

(define (cut-original-var var)
  (if (not (tmpvar? var)) var
      ;; This remove-varhash is for variables in inner expressions (still having hash)
      (remove-varhash (~a TMPTAG (car (string-split var TMPTAG))))))

(define (test-add-vars env)

  ;; '((varname hash_hash_.. expr) ...)
  (define ext
    (append-map
     (match-lambda
       [(list _ h1 _)
        (filter-map
         (match-lambda
           [(list _ h2 _)
            (and (number? h1) (number? h2) (< 0 h1 h2)
                 (list (format "~a~a_~a" TMPTAG h1 h2)
                       (format "~a_~a" h1 h2) "1"))])
         env)])
     env))

  (append ext env))

(define (ssa-code->xcodeml a)
  (define (rec n) (ssa-code->xcodeml n))
  (define (index n) `(arrayIndex ,n))

  (match a
    [(? number? _)
     (define s (number->string a))
     (if (exact-integer? a) `(FintConstant ,s) `(FrealConstant ,s)) ]

    [(? string? _) `(Var ,(cut-original-var a))]

    [(list (or 'st 'st2 'st3 'st4 'st5 'st6) name _ args ... expr)
     `(FassignStatement
       (FarrayRef (@ (type "none")) ; type is required but not actually used
        (varRef (Var ,name))
        ,@(map (compose index rec) args))
       ,(rec expr))]

    [(list (or 'ld 'ld2 'ld3 'ld4 'ld5 'ld6) name _ args ...)
     `(FarrayRef (@ (type "none"))
       (varRef (Var ,name))
       ,@(map (compose index rec) args))]

    [(list 'fma a b c) `(plusExpr ,(rec a) (mulExpr ,(rec b) ,(rec c)))]

    [(list 'mfma a b c) `(minusExpr ,(rec a) (mulExpr ,(rec b) ,(rec c)))]

    [(list 'asgFma a b c) `(asgPlusExpr ,(rec a) (mulExpr ,(rec b) ,(rec c)))]

    [(list 'asgMfma a b c) `(asgMinusExpr ,(rec a) (mulExpr ,(rec b) ,(rec c)))]

    [(list 'functionCall name args ...)
     (cond [(equal? name "__cast")
	    `(castExpr (@ (type ,(~ args 0))) ,(rec (~ args 1)))]

	   [(equal? name "__moe")
	    `(moeConstant (@ (type ,(~ args 0))) ,(~ args 1))]

	   [(equal? name "__power")
	    `(FpowerExpr (@ (type ,(~ args 0))) ,(rec (~ args 1)) ,(rec (~ args 2)))]

	   [(equal? name "__memberref")
	    `(memberRef (@ (member ,(~ args 1)) (type ,(~ args 2)) )
			,(sxml:set-attr (rec (~ args 0)) `(type ,(~ args 3)) ))]

	   [(equal? name "__varAddr")
	    `(varAddr ,(car (sxml:content (rec (~ args 0)))))]

	   [else
	    `(functionCall
          (@ (type "R1") (is_intrinsic "true"))
	      (name ,name)
	      (arguments ,@(map rec args)))])]

    [(list (and (or
                 'plusExpr 'minusExpr 'mulExpr 'divExpr 'modExpr 'condExpr
                 'LshiftExpr 'RshiftExpr 'bitAndExpr 'bitOrExpr 'bitXorExpr
                 'logEQExpr 'logNEQExpr 'logGEExpr 'logGTExpr 'logLEExpr
                 'logLTExpr 'logAndExpr 'logOrExpr 'unaryMinusExpr 'bitNotExpr
                 'asgPlusExpr 'asgMinusExpr 'asgMulExpr 'asgDivExpr)
                op) args ...)
     `(,op ,@(map rec args))]
    ))

;;
;; Put new compounds to declare extensional variables
;;
;;  New variables could refer other new ones via varname of env
;;     where env ::= ((varname hash_hash_.. expr) ...;
;;
;;  Therefore, env must contain new variables in the order of declaration.
;;
;;
;; Also, Update expressions with new ones
;;
(define (extend-node node env)
  (define extension
    (filter (compose string? (curryr ~ 1)) env))

  (define ext-vars (map car extension))
  (define var-to-asglist
    (map (lambda (x)
           (cons (~ x 0)
                 (map string->number (string-split (~ x 1) "_"))))
         extension))
  (define var-to-asglistH (make-hash var-to-asglist))
  (define var-to-expr (map (lambda (x) (cons (~ x 0) (~ x 2))) extension))

  ;; '((hash-of-compound hash-of-asg ...) ...)
  ;;   containing negligible a (hash-of-asg . hash-of-asg) map.
  ;;
  ;; `reverse` for ascending order (from smaller to bigger scopes)
  ;; Nested (asg contained in several relevant compounds)
  (define comp-to-asglist (reverse (decl-mapping node)))

  ;; Nested (asg contained in several relevants nodes)
  (define node-to-asglist (decl-mapping node #t))

  ;; '((ext-varname . minimum-scope) ...)
  (define var-to-comp
    (map
     (lambda (v)
       (define asglist (hash-ref var-to-asglistH v))
       (define s
         (find (compose null? (curry lset-difference = asglist))
               comp-to-asglist))
       (cons v (car s)))
     ext-vars))

  ;; '((hash-of-compound ext-varname ...) ...)
  ;; var contained only in the minimum compound
  (define comp-to-varlist (reverse-assoc var-to-comp))

  ;; '((hash-of-asg ext-varname ...) ...) ; var contained in several places
  (define asg-to-varlist (reverse-assoc-multi var-to-asglist))

  ;; '((hash-of-node ext-varname ...) ...) ; var contained in several places
  (define node-to-varlist
    (map
     (lambda (x)
       (cons
        (car x)
        (delete-duplicates
         (append-map (lambda (a) (assoc-ref asg-to-varlist a '())) (cdr x)))))
     node-to-asglist))

  (define var-to-type0
    (fold
     (lambda (pair alist)
       (assoc-adjoin alist (car pair) (cdr pair)))
     '()
     (append
      (map (lambda (v)
             (cons ((if-car-sxpath '(*text*)) v)
                   ((if-car-sxpath '(@ type *text*)) v)))
           ((sxpath "//Var") node))
      (map (lambda (a)
             (cons ((if-car-sxpath '(varRef Var *text*)) a)
                   ((if-car-sxpath '(@ type *text*)) a)))
           ((sxpath "//FarrayRef") node)))))

  (define var-to-type1
    (append-map
     (lambda (x)
       (define varname (lv-name (car (sxml:content x))))
       (define type (assoc-ref var-to-type0 varname))
       (define ref-tmpvars
         (filter-map (lambda (e)
                       (and (equal? (~ e 1) (eq-hash-code x))
                            ((conjoin string? tmpvar? remove-varhash) (~ e 2))))
                     env))
       (map (curryr cons type) ref-tmpvars))
     ((sxpath "//FassignStatement") node)))

  ;; todo:
  ;;  consider casting by storing
  ;;  (currently no local variables are referenced by temporary variables;
  ;;   thus, no such information is used -> partially supported now)
  ;;  consider type difference among register reuse or separate
  (define var-to-type
    (let loop ([v2t (append var-to-type0 var-to-type1)] [pending (map car var-to-expr)])
      (define ca (and (pair? pending) (car pending)))

      (cond [(not ca) v2t]

            ;; Cast by storing
            [(assoc-ref v2t ca)
             => (lambda (t)
                  (loop v2t (cdr pending)))]

            [(pair? (lset-intersection
                     equal? (get-var-dep ca (curry assoc-ref var-to-expr))
                     pending))
             (loop v2t (append (cdr pending) (list ca)))]

            [else ;; dependency solved
             (loop
              (assoc-adjoin v2t ca (type-reduct (infer-type (assoc-ref var-to-expr ca) v2t)))
              (cdr pending))])))

  (extend-compound node env
    (make-hash comp-to-varlist)
    (make-hash node-to-varlist)
    (make-hash var-to-expr)
    (make-hash var-to-type)))

(define (infer-type e v2t)
  (define (it x) (infer-type x v2t))

  (define (select-type types)
    (define order '("Fcomplex" "Freal" "Fint"))
    (or
     (find (conjoin values (curryr member order)) (sort types string>?))
     (find (curryr member types) order)
     "Freal"))

  (match e
    [(? number? _)
     (if (exact-integer? e) "Fint" "Freal")]

    [(? string? _)
     (assoc-ref v2t (cut-original-var e))]

    [(? (curryr sxml:attr 'type) type) type]

    [(list 'functionCall name args ...)
     (cond [(equal? name "__cast") (~ args 0)]

       [(equal? name "__power") (~ args 0)]

	   [(equal? name "__moe") "int"]

	   [(equal? name "__memberref") (~ args 2)]

	   [(equal? name "__varAddr") (~ args 1)]

	   ;; todo check declaration
	   [else (select-type (map it args))])]

    [(list (or 'ld 'ld2 'ld3 'ld4 'ld5 'ld6) name _ args ...)
     (assoc-ref v2t name)]

    [(list 'condExpr cond then else)
     (select-type (map it (list then else)))]

    [(list (and (or
                 'fma 'asgFma 'mfma 'asgMfma
                 'plusExpr 'minusExpr 'mulExpr 'divExpr 'modExpr
                 'LshiftExpr 'RshiftExpr 'bitAndExpr 'bitOrExpr 'bitXorExpr
                 'logEQExpr 'logNEQExpr 'logGEExpr 'logGTExpr 'logLEExpr
                 'logLTExpr 'logAndExpr 'logOrExpr 'unaryMinusExpr
                 'bitNotExpr
                 'asgPlusExpr 'asgMinusExpr 'asgMulExpr 'asgDivExpr)
                op) args ...)
     (select-type (map it args))]
    ))

(define (get-var-dep v expr-ref)
  (filter-map
   (conjoin string? tmpvar? remove-varhash)
   (flatten (expr-ref v))))

(define (extend-compound
         node env comp-to-varlist node-to-varlist var-to-expr var-to-type)
  (define (rec n)
    (extend-compound
     n env comp-to-varlist node-to-varlist var-to-expr var-to-type))

  ;; Decide the order of temp vars declared in the same spots
  (define (sort-vars vars)
    (define var-to-dep
      (map (lambda (v) (cons v (get-var-dep v (curry hash-ref var-to-expr))))
           vars))

    (define (previous v)
      (~a TMPTAG (string-join (drop-right (string-split v TMPTAG) 1) TMPTAG)))

    (let loop ([ret '()] [pending vars])
      (if (null? pending) ret
          (let ()
            (define-values
              (here there)
              (partition
               (lambda (x)
                 (and
                  ;; Dependency among vars solved?
                  (null? (lset-intersection
                          equal? vars
                          (lset-difference equal? (assoc-ref var-to-dep x) ret)))
                  ;; Not to overwrite variables still read by other vars
                  (not (member (previous x)
                               (append-map (curry assoc-ref var-to-dep)
                                           ;; srfi-1 overwrites remove
                                           (remove (curry equal? x) pending))))))
               pending))

            ;; Have loads first
            (define-values
              (loads rest)
              (partition
               (lambda (x)
                 (memq (car (hash-ref var-to-expr x))
                       '(ld ld2 ld3 ld4 ld5 ld6)))
               here))

            (define sorted-loads
              (sort
               loads
               (lambda (a b)
                 (define ea (hash-ref var-to-expr a))
                 (define eb (hash-ref var-to-expr b))
                 (if (not (equal? (~ ea 1) (~ eb 1)))
                     (string<? (~ ea 1) (~ eb 1))

                     (let ()
                       (define ia (drop ea 3))
                       (define ib (drop eb 3))
                       (define diff
                         (findf (negate (curry apply equal?)) (map list ia ib)))
                       (define da (~ diff 0))
                       (define db (~ diff 1))
		       (cond [(and (number? da) (number? db)) (< da db)]
			     [(and (number? da) (not (number? db))) #t]
			     [(and (number? db) (not (number? da))) #t]
			     [else (string<? da db)]))
                     ))))

            (loop (append ret sorted-loads rest) there)))))

  (define (build-compound nodes vars)
    (define-values (decl update) ; declared and overwritten
      (partition
       (compose (curry = 1) length (curryr string-split TMPTAG)) vars))

    ;; No initialization at declaration in order to keep vars sorted
    (set! update (sort-vars vars))

    (define update-stmts
      (map
       (lambda (v)
         (define orig (cut-original-var v))
         (define expr (ssa-code->xcodeml (hash-ref var-to-expr v)))
         `(FassignStatement (Var ,orig) ,expr))
       update))

    `(blockStatement
      (symbols
       ,@(map
          (lambda (v)
            `(id (@ (type ,(hash-ref var-to-type v)) (sclass "flocal") ) (name ,v)))
          decl))
      (declarations
       ,@(map
          (lambda (v)
            `(varDecl
              (name ,v)
              #;(value ,(ssa-code->xcodeml (hash-ref var-to-expr v)) )))
          decl))
      (body ,@update-stmts ,@nodes)))


  (define (build-body nodes ext-vars-ht)
    (if (null? nodes) '()
        (let ()
          (define n (car nodes))
          (define h (eq-hash-code n))
          (define vars (hash-ref node-to-varlist h))

          (define impl (filter (curry hash-update?! ext-vars-ht) vars))

          (define suc (cons (rec n) (build-body (cdr nodes) ext-vars-ht)))

          (if (null? impl) suc
              (list (build-compound suc impl))))))

  (define (list->marking-hash lst)
    (define ht (make-hash))
    (for-each (lambda (v) (hash-set! ht v #t)) lst)
    ht)

  (define (hash-mark! ht v)
    (hash-set! ht v #f))

  (define (hash-unmarked? ht v)
    (hash-ref ht v #f))

  (define (hash-update?! ht v)
    (and (hash-unmarked? ht v)
         (hash-mark! ht v)))

  (match (and (pair? node) (sxml:name node))
    ['body
     (define h (eq-hash-code node))
     (define ext-vars-ht (list->marking-hash (hash-ref comp-to-varlist h '())))

     ;; ext-vars for vars declared here
     (define here
       (filter
        (curry hash-update?! ext-vars-ht)
        (delete-duplicates
         (append-map
          (lambda (x) (hash-ref node-to-varlist (eq-hash-code x) '()))
          ((sxpath "declarations/varDecl") node)))))

     (for-each (curry hash-mark! ext-vars-ht) here)

     (define new-body (build-body (sxml:content node) ext-vars-ht))

     (define used '())

     (define vardecl-to-vars
       (map
        (lambda (x)
          (filter
           (lambda (v)
             (and (member v here) (not (member v used))
                  (set! used (cons v used))))
           (hash-ref node-to-varlist (eq-hash-code x) '())))
        ((sxpath "declarations/varDecl") node)))

     ;; Split declarations to insert additional ones and assigns
     (fold
      (lambda (vars s d b)
        (define r
          `(blockStatement
            (symbols ,s) (declarations ,d)
            (body ,b)))

        (if (null? vars) r (build-compound (list r) vars)))

      `(body ,@new-body)

      (reverse vardecl-to-vars)
      (reverse ((sxpath "symbols/*") node))
      (reverse (map rec ((sxpath "declarations/*") node))))]

    [(or 'FassignStatement 'varDecl
         'postIncrExpr 'postDecrExpr 'preIncrExpr 'preDecrExpr
         'asgPlusExpr 'asgMinusExpr 'asgMulExpr 'asgDivExpr
         'asgModExpr 'asgLshiftExpr 'asgRshiftExpr 'asgBitAndExpr
         'asgBitOrExpr 'asgBitXorExpr)
     (define r (map rec node)) ;; todo nest asg (make compound with the current asg)
     (define h (eq-hash-code node))

     ;; env is necessary because varlist of node-to-varlist could allow nests
     (define e (find (lambda (e) (equal? (~ e 1) h)) env))

     (if (not e) r
         (let ()
           (define out (ssa-code->xcodeml (~ e 2)))
           (cond [(eq? (car out) 'FassignStatement) out] ; in the case of stN

                 [(eq? (sxml:name node) 'varDecl)
                  `(varDecl (name ,(~ e 0)) (value ,out))]

                 [else
                  (if (and (eq? (car out) 'Var) (equal? (cadr out) (~ e 0)))
                      '(FintConstant "0") ; for removed stN
                      `(FassignStatement (Var ,(~ e 0)) ,out))]
                 )))]

    [#f node]

    [else (map rec node)]))

;; '((hash-of-compound hash-of-asg ...) ...)
(define (decl-mapping node [include-all #f] [nest #t])
  (define (rec n)
    (decl-mapping n include-all nest))

  (match (and (pair? node) (sxml:name node))
    [(or 'FassignStatement 'varDecl
         'postIncrExpr 'postDecrExpr 'preIncrExpr 'preDecrExpr
         'asgPlusExpr 'asgMinusExpr 'asgMulExpr 'asgDivExpr
         'asgModExpr 'asgLshiftExpr 'asgRshiftExpr 'asgBitAndExpr
         'asgBitOrExpr 'asgBitXorExpr)
     (define r (append-map rec node))
     (define h (eq-hash-code node))
     (cons (list h h) r)]

    [#f '()]

    [else
     (define r (append-map rec node))
     (define h (eq-hash-code node))

     (if (not (or include-all
                  (eq? (sxml:name node) 'body)
                  )) r
         (let ()
           (define-values (comp asg)
             (partition 
              (conjoin (compose (curry <= 2) length)
                       (compose not (curry apply =) (curryr take 2))) r))

           (define n (cons h (append-map cdr asg)))
           (define m (if (not nest) n (append n (append-map cdr comp))))

           (cons (delete-duplicates m) (if include-all r comp))))]))

(define (reduce-st-chain env)
  (map
   (lambda (x)
     (if (and (pair? (~ x 2)) (memq (car (~ x 2)) '(st st2 st3 st4 st5 st6)))
         ;; refer to original env not to reflect reduced st
         (list (~ x 0) (~ x 1) (opt-dep-st (~ x 2) env))
         x))
   env))

(define (delete-unnecessary-phi env)
  (define new-env (delete-unnecessary-phi-once env))
  (if (equal? env new-env) env
      (delete-unnecessary-phi new-env)))

(define (delete-unnecessary-phi-once env)
  (define terms (flatten env))
  (filter
   (lambda (i)
     (define name (format "~a__~a" (~ i 0) (~ i 1)))
     (define expr (~ i 2))
     (define phi (and (pair? expr) (eq? (car expr) 'phi)))
     (or (not phi) (member name terms)))
   env))

(define TMPHASH_COUNT 0)
(define (sat-temporary-hash)
  (set! TMPHASH_COUNT (- TMPHASH_COUNT 1))
  TMPHASH_COUNT)

;; env ::= '((var hash expr) ..)
(define (extract-ssa node [env '()])
  (define (rec n)
    (extract-ssa n env))

  (define (mrec ns [env env])
    (if (null? ns) (values env '())

        (let*-values ([(top-env top-n) (extract-ssa (car ns) env)]
                      [(suc-env suc-n) (mrec (cdr ns) top-env)])
          (values suc-env (cons top-n suc-n)))))

  (match (sxml:name node)
    [(or 'OMPPragma 'ACCPragma) (rec (~ (sxml:content node) 2))]

    ['varDecl
     (define name ((if-car-sxpath "name/text()") node))
     (define value ((if-car-sxpath "value/*") node))

     (define h (eq-hash-code node))
     (define-values (_ new-value) (if value (rec value) (values #f #f)))
     (define new-env
       (if (and name value) (cons (list name h new-value) env) env))

     (values new-env #f)]

    ['blockStatement
     (mrec
      (append
       ((sxpath "declarations/varDecl") node)
       ((sxpath "body/*") node)))]

    ['FdoStatement
     (define body ((sxpath "body/*") node))
     (define-values (body-env0 _) (mrec body))
     (define iterators
       ((sxpath '(Var *text*)) node))
     (define diff (lset-difference equal? body-env0 env))
     (define diff-vars (delete-duplicates (append iterators (map car diff))))

     (define pre-phi-env
       (map
        (lambda (var)
          (define h (sat-temporary-hash))
          (define expr
            `(phi ,var
                  ,(format "for__~a_pre" (eq-hash-code node))
                  ,(format "for__~a_cont" (eq-hash-code node))
                  ,(rename var env)))
          (list var h expr))
        diff-vars))

     (define body-env1 (append pre-phi-env env))
     (define-values (body-env __) (mrec body body-env1))

     (define cond-expr (format "for__~a" (eq-hash-code node)))

     (define phi-env
       (map
        (lambda (var)
          (define b (assoc-ref body-env var))
          (define o (assoc-ref body-env1 var))
          (define h (sat-temporary-hash))
          (define expr
            `(phi ,var ,cond-expr
                  ,(rename var body-env) ,(rename var body-env1)))
          (and expr (list var h expr)))
        diff-vars))

     (values (append phi-env body-env) #f)]

    ['FdoWhileStatement
     (mrec ((sxpath "body/*") node))]

    ['FifStatement
     (define condition ((if-car-sxpath "condition/*") node))
     (define then ((sxpath "then/body/*") node))
     (define else ((sxpath "else/body/*") node))

     (define-values (cond-env0 cond-expr) (rec condition))
     (define cond-env env)
     (define-values (then-env0 _) (mrec then cond-env))
     (define-values (else-env0 __)
       (if (pair? else) (mrec else cond-env) (values cond-env #f)))

     ;; Bound computation within conditional scopes
     (define then-updated (map car (lset-difference equal? then-env0 cond-env)))
     (define else-updated (map car (lset-difference equal? else-env0 cond-env)))
     (define then-phi (map (lambda (x)
			     (list x (sat-temporary-hash)
				   `(phi ,x 1 ,(~ (assoc-ref cond-env x `(0 ,x))
						  1) ,x))) then-updated))
     (define else-phi (map (lambda (x)
			     (list x (sat-temporary-hash)
				   `(phi ,x 1 ,(~ (assoc-ref cond-env x `(0 ,x))
						  1) ,x))) else-updated))
     (define-values (then-env ___) (mrec then (append then-phi cond-env)))
     (define-values (else-env ____)
       (if (pair? else) (mrec else (append else-phi cond-env)) (values cond-env #f)))

     (define then-vars (map car then-env))
     (define else-vars (map car else-env))
     (define vars (delete-duplicates (append then-vars else-vars)))

     ;; phi is ignored in code generation;
     ;; It works just as a pointer to an e-node.
     ;; phi prevents invalid code sharing unless the condition allows rewrites
     (define phi-env
       (filter-map
        (lambda (var)
          (define t (assoc-ref then-env var))
          (define e (assoc-ref else-env var))
          (define h (sat-temporary-hash))
          (define expr
            (and (not (and t e (equal? t e)))
                 `(phi ,var ,cond-expr
                       ,(rename var then-env)
                       ,(rename var else-env))))
          (and expr (list var h expr)))
        vars))

     (define diff-env (lset-difference equal? else-env then-env))

     (values (append phi-env diff-env then-env) #f)]

    ['exprStatement
     (rec (car (sxml:content node)))]

    ['FassignStatement
     (match-define (list lv rv) (sxml:content node))
     (define-values (renv new-rv) (rec rv))

     (define name (lv-name lv))
     (define h (eq-hash-code node))

     (unless (member (sxml:name lv) '(Var FarrayRef))
       (error (~a lv)))

     (define-values (new-env new-addrs)
       (match (sxml:name lv)
         ['FarrayRef
          (define addrs ((sxpath "arrayIndex/*") lv))
          (mrec addrs renv)]

         [else (values renv #f)]))

     (define len (and new-addrs (length new-addrs)))
     (define st
       (and new-addrs
            (if (= len 1) 'st (string->symbol (format "st~a" len)))))
     (define expr
       (if (not st) new-rv
           `(,st ,name ,(rename name new-env) ,@new-addrs ,new-rv)))

     (values (cons (list name h expr) new-env) new-rv)]

    ['functionCall
     (define-values (new-env expr)
       (mrec (sxml:content ((if-car-sxpath "arguments") node))))

     ;; todo support reference arguments
     (values new-env
             `(functionCall ,((if-car-sxpath "name/text()") node)
                            ,@expr))]

    ['FpowerExpr
     (define-values (new-env expr) (mrec (sxml:content node)))
     (values new-env `(functionCall "__power" ,(sxml:attr node 'type) ,@expr))]

    [(or 'plusExpr 'minusExpr 'mulExpr 'divExpr 'condExpr 'modExpr
         'LshiftExpr 'RshiftExpr 'bitAndExpr 'bitOrExpr 'bitXorExpr
         'logEQExpr 'logNEQExpr 'logGEExpr 'logGTExpr 'logLEExpr
         'logLTExpr 'logAndExpr 'logOrExpr 'unaryMinusExpr 'bitNotExpr
         'logNotExpr 'sizeOfExpr 'commaExpr 'commaExpr0)
     (define-values (new-env expr) (mrec (sxml:content node)))
     (values new-env (cons (sxml:name node) expr))]

    ['FarrayRef
     (define addrs ((sxpath "arrayIndex/*") node))
     (define-values (new-env new-addrs) (mrec addrs))

     (define len (length addrs))
     (define ld (if (= len 1) 'ld (string->symbol (format "ld~a" len))))
     (define name (lv-name node))

     (values env
             (opt-dep-ld `(,ld ,name ,(rename name new-env) ,@new-addrs) new-env))]

    ['Var
     (define name (rename (lv-name node) env))
     (values env name)]

    [(or 'FintConstant 'FlogicalConstant 'FrealConstant)
     (values env (string->number (sxml:text node)))]

    [(or 'caseLabel 'defaultLabel 'breakStatement 'continueStatement
         'stringConstant 'funcAddr 'arrayAddr 'FpragmaStatement)
     (values env (sxml:snip node))]

    [else (error (~a node))]))

(define (opt-dep dep addrs env)
  (define (diff? pre-addrs)
    (any (lambda (x)
           (define a (car x))
           (define b (cdr x))
           (and (number? a) (number? b) (not (= a b))))
         (map cons addrs pre-addrs)))

  (if (equal? (remove-varhash dep) dep) dep
      (let ()
        (define hash (string->number (~ (string-split dep "__") 1)))
        (define tail (memf (lambda (x) (= hash (~ x 1))) env))
        (define st (last (car tail)))
        (if (and (>= hash 0)
                 (not (eq? (car st) 'phi)) ;; it's st?
                 (diff? (drop (drop-right st 1) 3)))
            (opt-dep (~ st 2) addrs (cdr tail))
            dep))))

(define (opt-dep-ld x env)
  (define op   (~ x 0))
  (define name (~ x 1))
  (define dep  (~ x 2))
  (define addrs (drop x 3))
  `(,op ,name ,(opt-dep dep addrs env) ,@addrs))

(define (opt-dep-st x env)
  (define op   (~ x 0))
  (define name (~ x 1))
  (define dep  (~ x 2))
  (define addrs (drop (drop-right x 1) 3))
  (define rv (last x))
  `(,op ,name ,(opt-dep dep addrs env) ,@addrs ,rv))

(define (rename x env)
  (define ref (assoc-ref env x))
  (if (not ref) x
      (format "~a__~a" x (car ref))))

(define (lv-name x)
  (define cc (lambda (a) (car (sxml:content a))))
  (match (sxml:name x)
    ['Var (cc x)]
    ['FarrayRef (cc (cc (cc x)))]
    ))
