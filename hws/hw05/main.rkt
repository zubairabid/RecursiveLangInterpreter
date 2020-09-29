#lang racket

(define-syntax colist
  (syntax-rules ()
    [(colist sym terms ...) 
     (lambda () (list sym terms ...))]))

;;; unroll :: term? -> term?
(define (unroll t)
  (if (procedure? t) (t) t))
;;; hd :: term? -> symbol?
(define (hd t)
  (car (unroll t)))

;;; tl :: term? -> term?
(define (tl t)
  (cdr (unroll t)))
(struct exn:type-error exn:fail ())

(define (raise-type-error)
  (raise (exn:type-error
          "Invalid Arguments"
          (current-continuation-marks))))
;;; term? -> term? -> boolean?
(define (bisimilar? t1 t2)
 ) ;; your solution here.

(provide bisimilar?)
(provide colist)
(provide (struct-out exn:type-error))
