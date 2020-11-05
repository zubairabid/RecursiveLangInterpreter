#lang racket

(require eopl)

(define-datatype ast ast?
  [num (datum number?)]
  [bool (datum boolean?)]
  [ifte (test ast?) (then ast?) (else-ast ast?)]
  [function
   (formals (list-of id?))
   (body ast?)]
  [recursive (fbinds (list-of fbind?)) (body ast?)]
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
(define-datatype fbind fbind?
  [make-fbind (fb-id id?)
              (fb-formals (list-of id?))
              (fb-body ast?)])

;;; fbind-id : fbind? -> id?
(define fbind-id
  (lambda (b)
    (cases fbind b
      [make-fbind (fb-id fb-formals fb-body) fb-id])))

;;; fbind-formals : fbind? -> (list-of id?)
(define fbind-formals
  (lambda (b)
    (cases fbind b
      [make-fbind (fb-id fb-formals fb-body) fb-formals])))

;;; fbind-body : fbind? -> ast?
(define fbind-body
  (lambda (b)
    (cases fbind b
      [make-fbind (fb-id fb-formals fb-body) fb-body])))
(define *keywords*
  '(ifte assume function recursive))

(define id?
  (lambda (x)
    (and
     (symbol? x)
     (not (memq x *keywords*)))))

;;; parse :: any/c -> ast?  Raises exception exn?
;;; Fill in the function parse here
(define (parse exp)
  ;; complete the definition
  #f)

;;(define-datatype env env?
;;  [empty-env]
;;  [extended-env
;;    (syms (list-of symbol?))
;;    (vals (list-of denotable-value?))
;;    (outer-env env?)]
;;  [extended-rec-env
;;    (fsyms (list-of symbol?))
;;    (lformals (list-of (list-of symbol?)))
;;    (bodies (list-of ast?))
;;    (outer-env env?)])
;;(define-datatype proc proc?
;;  [prim-proc
;;    ;; prim refers to a scheme procedure
;;    (prim procedure?)
;;    ;; sig is the signature
;;    (sig (list-of procedure?))] 
;;  [closure
;;    (formals (list-of symbol?))
;;    (body ast?)
;;    (env env?)])
;;
;;;;; prim? : proc? -> boolean?
;;(define prim-proc?
;;  (lambda (p)
;;    (cases proc p
;;      [prim-proc (prim sig) #t]
;;      [else #f])))
;;
;;(define closure? 
;;  (lambda (p)
;;    (cases proc p
;;      [prim-proc (prim sig) #f]
;;      [else #t])))
;;;;; expressible-value? : any/c -> boolean?
;;(define expressible-value?
;;  (or/c number? boolean? proc?))
;;;;; denotable-value? :any/c -> boolean?
;;(define denotable-value?
;;  (or/c number? boolean? proc?))
;;;;; empty-env? : env? -> boolean?
;;(define empty-env?
;;  (lambda (e)
;;    (cases env e
;;      [empty-env () #t]
;;      [else #f])))
;;
;;;;; extended-env? : env? -> boolean?
;;(define extended-env?
;;  (lambda (e)
;;    (cases env e
;;      [extended-env (syms vals outer-env) #t]
;;      [else #f])))
;;
;;;;; extended-rec-env? : env? -> boolean?
;;(define extended-rec-env?
;;  (lambda (e)
;;    (cases env e
;;      [extended-rec-env (fsyms lformals bodies outer-env) #t]
;;      [else #f])))
;;(define lookup-env
;;  (lambda (e x) 
;;    #f))
;;;;; implement all procedures in the list
;;(define +p ...)
;;(define eval-ast
;;  (lambda (a e)
;;    ;; your solution here
;;    #f))
;;
;;(define *init-env*
;;  (extended-env
;;   '(+ - * / < <= eq? 0? !)
;;   (list +p -p *p /p <p <=p eq?p 0?p !p)
;;   (empty-env)))


(provide (all-defined-out))
