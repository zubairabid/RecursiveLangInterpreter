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

;;; exporting only the required function
(provide repeat)
(provide invert)
(provide count-occurrences)
