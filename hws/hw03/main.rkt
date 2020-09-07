#lang racket
(require eopl)
(require rackunit)
(require racket/match)
(provide (all-defined-out))


(define-datatype ast ast?
 [binop (op binop?) (rand1 ast?) (rand2 ast?)]
 [ifte (c ast?) (t ast?) (e ast?)]
 [num (n number?)]
 [bool (b boolean?)])
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
(struct exn:parse-error exn:fail ())
(define raise-parse-error 
 (lambda (err-msg)
   (raise (exn:parse-error err-msg (current-continuation-marks)))))
;;; parse :: any/c -> ast?  Raises exception exn:parse-error?
;;; Fill in the function parse here
(define (parse exp)
  (cond [(number? exp) (num exp)]
        [(boolean? exp) (bool exp)]
        [(and (list? exp)
              (= (length exp) 4)
              (eq? (first exp) 'if))
         (ifte (parse (second exp))
               (parse (third exp))
               (parse (fourth exp)))]
        [(and (list? exp)
              (= (length exp) 3)
              (eq? (first exp) '+))
         (binop 'add (second exp) (third exp))]
        [else (raise-parse-error "invalid syntax")]) ;; add an else
  )

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
      ['lt? <]
      ['eq? =]
      [_ error 'op-interpretation "unknown op"])))
;;; eval-ast :: ast? -> expressible? || (or/c exn:exec-div-by-zero  exn:exec-type-mismatch)
(define eval-ast
  (lambda (a)
    a
    ))
(define ts-numop-incorrect-param-rand1
  (test-suite 
   "wrongly typed rand1 parameters"
   (for/list ([numerical-op '(add sub mul div lt? eq?)])
     (test-case (string-append (symbol->string numerical-op) "-type-mismatch-rand1")
       (check-exn exn:exec-type-mismatch?
                  (lambda () 
                    (eval-ast (binop numerical-op
                                     (binop 'lt? (num 10) (num 20)) ; boolean
                                     (num 10)))))))))

(define ts-numop-incorrect-param-rand2
  (test-suite
   "wrongly typed rand2 parameters"
   (for/list ([numerical-op '(add sub mul div)])
     (test-case (string-append (symbol->string numerical-op) "-type-mismatch-rand1")
       (check-exn exn:exec-type-mismatch?
                  (lambda () 
                    (eval-ast (binop numerical-op (num 10)
                                     (binop 'lt? (num 10) (num 20))))))))))
