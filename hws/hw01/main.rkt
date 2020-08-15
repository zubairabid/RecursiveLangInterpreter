#lang racket/base

(define (repeat n x)
  (if (= n 0)
      '()
      (cons x (repeat (- n 1) x))))
(define (invert lst)
  (if (null? lst)
      '()
      (cons (reverse (car lst)) (invert (cdr lst)))))
(define (count-occurrences s slist)
  (define (occplus s comp)
    (if (= s comp)
        1
        0))
  (if (null? slist)
      0
      (+ (occplus s (car slist)) (count-occurrences s (cdr slist)))))
(define (product sos1 sos2) 
  (cond
    [(null? sos1) sos2]
    [(null? sos2) sos1]
    [else 
      (cond
        [(> (length sos1) 1)
         (append (product (list (car sos1)) sos2) (product (cdr sos1) sos2))]
        [(> (length sos2) 1)
         (append (product sos1 (list (car sos2))) (product sos1 (cdr sos2)))]
        [else (list (append sos1 sos2))])]))
(define (every pred lst) 
  (if (null? lst)
      #t
      (and (pred (car lst)) (every pred (cdr lst)))))

;;; exporting only the required function
(provide repeat)
(provide invert)
(provide count-occurrences)
(provide product)
(provide every)
