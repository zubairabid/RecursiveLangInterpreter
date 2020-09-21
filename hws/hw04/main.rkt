#lang racket

(require eopl)
(require rackunit)
(require racket/match)
(provide (all-defined-out))


(define-datatype ast ast?
  [unaryop (op unaryop?) (rand ast?)]
  [binop (op binop?) (rand1 ast?) (rand2 ast?)]
  [ifte (c ast?) (t ast?) (e ast?)]
  [num (n number?)]
  [bool (b boolean?)]
  [id-ref (sym id?)]
  [assume (bindings (list-of bind?)) (body ast?)])
(define-datatype bind bind?
  [make-bind (b-id id?) (b-ast ast?)])

(define id? symbol?)
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
;;; expressible-value? : any/c -> boolean?
(define expressible-value?
  (lambda (thing)
    (or (number? thing)
      (boolean? thing))))
;;; denotable-value? :any/c -> boolean?
(define denotable-value?
  (lambda (thing)
    (or (number? thing)
      (boolean? thing))))
(struct exn:parse-error exn:fail ())
(define raise-parse-error 
 (lambda (err-msg)
   (raise (exn:parse-error err-msg (current-continuation-marks)))))
;;; parse :: any/c -> ast?  Raises exception exn:parse-error?
;;; Fill in the function parse here
(define (parse exp)
  (cond [(number? exp) (num exp)]       ;; number parser
        [(boolean? exp) (bool exp)]     ;; boolean parser
        [(id? exp) (id-ref exp)]         ;; symbol parser
        [(and (list? exp)               ;; ifte parser
              (= (length exp) 4)
              (eq? (first exp) 'if))
         (ifte (parse (second exp))
               (parse (third exp))
               (parse (fourth exp)))]
        [(and (list? exp)               ;; math (binop) parser
              (= (length exp) 3))
         (match (first exp)
                ['+ (binop 'add (parse (second exp)) (parse (third exp)))]
                ['- (binop 'sub (parse (second exp)) (parse (third exp)))]
                ['* (binop 'mul (parse (second exp)) (parse (third exp)))]
                ['/ (binop 'div (parse (second exp)) (parse (third exp)))]
                ['< (binop 'lt? (parse (second exp)) (parse (third exp)))]
                ['== (binop 'eq? (parse (second exp)) (parse (third exp)))]
                [_ (raise-parse-error "Parse error")])]
        [(and (list? exp)               ;; unaryop parser
              (= (length exp) 2)
              (eq? (first exp) `!))
         (unaryop 'neg (parse (second exp)))]
        [else (raise-parse-error "Parse error")]) ;; add an else
  )
;;;;; parse :: any/c -> ast?  Raises exception exn:parse-error?
;;;;; Fill in the function parse here
;;(define (parse exp)
;;  ;; complete the definition
;;  1)
(define-datatype env env?
  [empty-env]
  [extended-env
    (syms (list-of symbol?))
    (vals (list-of denotable-value?))
    (outer-env env?)])
;;; empty-env? : env? -> boolean?
(define empty-env?
  (lambda (e)
    (cases env e
      [empty-env () #t]
      [else #f])))
;;; extended-env? : env? -> boolean?
(define extended-env?
  (lambda (e)
    (cases env e
      [empty-env () #f]
      [else #t])))
;;; lookup-env: [env?  symbol?] -> any/c || exn:lookup-err?
(define lookup-env
  (lambda (e x)
    1)) ;; your solution here.
(struct exn:lookup-error exn:fail ())
(define raise-lookup-error 
  (lambda ()
    (raise (exn:lookup-error "unbound identifier" (current-continuation-marks)))))
(struct exn:exec-div-by-zero exn:fail ())
(define raise-exec-div-by-zero
  (lambda ()
    (raise (exn:exec-div-by-zero "div-by-0!" (current-continuation-marks)))))

(struct exn:exec-type-mismatch exn:fail ())
(define raise-exec-type-mismatch
  (lambda ()
    (raise (exn:exec-type-mismatch "type mismatch!" (current-continuation-marks)))))
;;; runtime-check :: [expressible? -> boolean?], exn? -> [expressible? -> expressible? || exn?] 
(define runtime-check
  (lambda (pred? exn)
    (lambda (v)
      (if (pred? v)
          v
          (exn)))))

(define typecheck-num
  (runtime-check number?  raise-exec-type-mismatch))

(define typecheck-bool 
  (runtime-check boolean? raise-exec-type-mismatch))

(define check-non-zero
  (runtime-check (not/c zero?) raise-exec-div-by-zero))
(define op-interpretation
  (lambda (op)
    (match op
      ['add +]
      ['sub -]
      ['mul *]
      ['div /]
      ['lt? <]
      ['eq? =]
      ['neg not]
      [_ error 'op-interpretation "unknown op"])))

;;; eval-ast :: [ast? env?] -> expressible-value? 
;;;                         || (or/c exn:exec-div-by-zero  exn:exec-type-mismatch exn:lookup-error)
(define eval-ast
  (lambda (a e)
    ;; your solution here
    1))
(define unaryop?
  (lambda (x)
    (match x
      ['neg #t]
      [_ #f])))
(define binop?
  (lambda (x)
    (match x
      ['add #t]
      ['sub #t]
      ['mul #t]
      ['div #t]
      ['lt? #t]
      ['eq? #t]
      [_ #f])))
(define ts-numop-incorrect-param-rand1
  (test-suite 
   "wrongly typed rand1 parameters"
   (for/list ([numerical-op '(add sub mul div lt? eq?)])
     (test-case (string-append (symbol->string numerical-op) "-type-mismatch-rand1")
       (check-exn exn:exec-type-mismatch?
                  (lambda () 
                    (eval-ast (binop numerical-op
                                     (binop 'lt? (num 10) (num 20)) ; boolean
                                     (num 10))
                              (empty-env))))))))

(define ts-numop-incorrect-param-rand2
  (test-suite
   "wrongly typed rand2 parameters"
   (for/list ([numerical-op '(add sub mul div)])
     (test-case (string-append (symbol->string numerical-op) "-type-mismatch-rand1")
       (check-exn exn:exec-type-mismatch?
                  (lambda () 
                    (eval-ast (binop numerical-op (num 10)
                                     (binop 'lt? (num 10) (num 20)))
                              (empty-env))))))))
