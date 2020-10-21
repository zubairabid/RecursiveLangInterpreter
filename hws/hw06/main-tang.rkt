#lang racket

(require eopl)

(define-datatype ast ast?
  [num (datum number?)]
  [bool (datum boolean?)]
  [ifte (test ast?) (then ast?) (else-ast ast?)]
  [function
   (formals (list-of id?))
   (body ast?)]
  [app (rator ast?) (rands (list-of ast?))]
  [id-ref (sym id?)]
  [assume (binds  (list-of bind?)) (body ast?)])

(define-datatype bind bind?
  [make-bind (b-id id?) (b-ast ast?)])

;;; bind-id : bind? -> id?
(define bind-id
  (lambda (b)
    (cases bind b
      [make-bind (b-id b-ast) b-id])))

;;; bind-ast : bind? -> ast?
(define bind-ast
  (lambda (b)
    (cases bind b
      [make-bind (b-id b-ast) b-ast])))
(define env? procedure?)


;;; lookup-env: [env?  symbol?] -> any/c
;;; lookup-env: throws "unbound identifier" error
(define lookup-env
  (lambda (e x)
    (e x)))

;;; empty-env : () -> env?
(define empty-env
  (lambda ()
    (lambda (x)
      (error 'empty-env "unbound identifier ~a" x))))

;;; extended-env :
;;;    [(list-of symbol?) (list-of any/c) env?] -> env?
(define extended-env
  (lambda (syms vals outer-env)
    (lambda (x)
      (let ([j (list-index syms x)])
        (cond
          [(= j -1) (lookup-env outer-env x)]
          [#t (list-ref vals j)])))))

;;; Returns the loction of the element in a list, -1 if the
;;; element is absent.

;;; list-index : [(listof any/c)  any/c] -> 
(define list-index
  (lambda (ls a)
    (letrec ([loop
               (lambda (ls ans)
                 (cond
                   [(null? ls) -1]
                   [(eq? (first ls) a) ans]
                   [#t (loop (rest ls) (+ 1 ans))]))])
      (loop ls 0))))
(define *keywords*
  '(ifte function assume))

(define id?
  (lambda (x)
    (and
     (symbol? x)
     (not (memq x *keywords*)))))

;;; parse :: any/c -> ast?  Raises exception exn?
;;; Fill in the function parse here
(define (parse exp)
  (define bindparse             ;; helper function, parses assumes
    (lambda (expr)
      (if (empty? expr)
          '()
          (let (
                [var (first (first expr))]
                [ex (parse (second (first expr)))])
            (cons 
              (make-bind var ex) 
              (bindparse (rest expr)))))))
  (define astlistparse
    (lambda (listexp)
      (if (null? listexp)
          '()
          (cons (parse (first listexp)) (astlistparse (rest listexp))))))
  (cond [(number? exp) (num exp)]       ;; number parser
        [(boolean? exp) (bool exp)]     ;; boolean parser
        [(id? exp) (id-ref exp)]        ;; symbol parser
        [(and (list? exp)               ;; ifte parser
              (= (length exp) 4)
              (eq? (first exp) 'if))
         (ifte (parse (second exp))
               (parse (third exp))
               (parse (fourth exp)))]
        [(and (list? exp)
              (= (length exp) 3)
              (eq? (first exp) 'assume))
         (assume                        ;; generic assume parser
           (bindparse (second exp))
           (parse (third exp)))]
        [(and (list? exp)
              (= (length exp) 3)
              (eq? (first exp) 'function))
         (function
           (second exp)
           (parse (third exp)))]
        [(list? exp)
         (app
           (parse (first exp))
           (astlistparse (rest exp)))]) ;; add an else
  )
(define-datatype proc proc?
  [prim-proc
    ;; prim refers to a scheme procedure
    (prim procedure?)
    ;; sig is the signature
    (sig (list-of procedure?))] 
  [closure
    (formals (list-of symbol?))
    (body ast?)
    (env env?)])

;;; prim? : proc? -> boolean?
(define prim-proc?
  (lambda (p)
    (cases proc p
      [prim-proc (prim sig) #t]
      [else #f])))

(define closure? 
  (lambda (p)
    (cases proc p
      [prim-proc (prim sig) #f]
      [else #t])))
;;; expressible-value? : any/c -> boolean?
(define expressible-value?
  (or/c number? boolean? proc?))
;;; denotable-value? :any/c -> boolean?
(define denotable-value?
  (or/c number? boolean? proc?))
;;; implement all procedures in the list
(define +p
    (prim-proc + (list number? number? number?)))

(define -p
    (prim-proc - (list number? number? number?)))

(define *p
    (prim-proc * (list number? number? number?)))

(define /p
    (prim-proc / (list number? number? number?)))

(define <p
    (prim-proc < (list boolean? number? number?)))

(define <=p
    (prim-proc <= (list boolean? number? number?)))

(define eq?p
    (prim-proc eq? (list boolean? number? number?)))

(define 0?p
    (prim-proc zero? (list boolean? number?)))

(define !p
    (prim-proc not (list boolean? boolean?)))
(define *init-env*
  (extended-env
   '(+ - * / < <= eq? 0? !)
   (list +p -p *p /p <p <=p eq?p 0?p !p)
   (empty-env)))
(define eval-ast
  (lambda (a e)
    (define (envfrombind binds)
      (if (= (length binds) 1)
          (extended-env
            (list (bind-id (first binds)))
            (list (eval-ast (bind-ast (first binds)) e))
            e)
          (extended-env
            (list (bind-id (first binds)))
            (list (eval-ast (bind-ast (first binds)) e))
            (envfrombind (rest binds)))))
    (cases ast a
           [num (n) n]
           [bool (b) b]
           [id-ref (sym) (lookup-env e sym)]
           [function (formals body) 
                     (closure
                       formals
                       body
                       e)]
           [app (rator rands)
                (cases proc rator
                       [prim-proc (prim sig) 1]
                       [closure (formals body env) 1])]
           [ifte (c th el)
                 (if (boolean? (eval-ast c e))
                     (eval-ast th e)
                     (eval-ast el e))]
           [assume (binds expr)
                   (eval-ast expr (envfrombind binds))]
           [else 5])))


(provide (all-defined-out))
