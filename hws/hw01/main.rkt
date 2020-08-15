#lang racket/base

(define (repeat n x)
  (if (= n 0)
      '()
      (cons x (repeat (- n 1) x))))
(define (invert lst)
  (if (null? lst)
      '()
      (cons (reverse (car lst)) (invert (cdr lst)))))

;;; exporting only the required function
(provide repeat)
(provide invert)
