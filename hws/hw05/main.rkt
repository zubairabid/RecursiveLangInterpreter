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
  (define bisimlist (make-hash))
  (if (and (null? t1)
           (null? t2))
      (raise-type-error); send error
      (bisim? t1 t2 bisimlist))) ;; your solution here.

(define (bisim? t1 t2 bisimlist)
  (cond [(and (null? t1) ;; Covering base cases: first, both empty
              (null? t2)) #t]
        ;; If either is null while the other isn't then it's not bisimilar

        [(or (and (null? t1)
                  (not (null? t2)))
             (and (not (null? t1))
                  (null? t2)))
         #f]

        ;; If either of the terms is a procedure while the other isn't, we need 
        ;; to unroll it for better comparison
        ;; Reminder: the head of the term will be a procedure,
           ;; and we can covert it to hd tl of hd using unroll. Expand within
           ;; the list, so use append to add it to the tl 
        [(and (not (procedure? (hd t1)))
              (procedure? (hd t2)))
         (bisim? 
           t1
           (append (unroll (hd t2)) (tl t2))
           bisimlist)]
        [(and (procedure? t1)
              (not (procedure? (hd t2))))
         (bisim?
           (append (unroll (hd t1)) (tl t1))
           t2
           bisimlist)]

        ;; If both are procedures, then check if they already exist in bisimlist
        ;; If yes, return #t. Else call the function again with unrolled like
        ;; with the one-colist but with both
        [(and (procedure? (hd t1))
              (procedure? (hd t2)))
         (if (hash-has-key? bisimlist (cons '(hd t1) '(hd t2)))
             (bisim? (tl t1) (tl t2) bisimlist)
             (bisim?
               (append (unroll (hd t1)) (tl t1))
               (append (unroll (hd t2)) (tl t2))
               bisimlist))
         ]

        ;; If both are lists, compare heads. If they match, add t1 and t2 to the
        ;; bisimlist and return bisim? on rest. Else, return false
        [else 
          (if (equal? (hd t1) (hd t2))
              (bisim?
                (tl t1)
                (tl t2)
                (hash-set! bisimlist (cons '(t1) '(t2)) 1))
              #f)]))

(provide bisimilar?)
(provide colist)
(provide (struct-out exn:type-error))
