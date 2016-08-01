#lang racket
(define x1 (list (list 2 3) (list 1 (list 4 6) 4)))
(define x2 (list 4 6 7 4 2 3 ))
(define x3 (list 1 3 2 4 5))
(define x4 (list (list 1 3 4) (list 4 5 6) (list 7 8 9) (list 9 10 11)))

(define (accumulate operate initial sequence)
  (cond ((null? sequence)
         initial)
        (else
         (operate (car sequence)
                  (accumulate operate initial (cdr sequence))))))

(define (accumulate-n operate initial sequence)
  (cond ((null? (car sequence))
         null)
        (else
         (cons (accumulate operate initial (map car sequence))
               (accumulate-n operate initial (map cdr sequence))))))