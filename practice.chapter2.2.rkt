#lang racket
(define v1 (list 1 2 3 4 5))
(define v2 (list 2 3 4 5 6))
(define m1 (list (list 1 2 3 4 5)
                 (list 2 3 4 5 6)
                 (list 3 4 5 6 7)
                 (list 4 5 6 7 8)
                 (list 5 6 7 8 9)))
; accumulate 的实现
(define (accumulate operate initial sequence)
  (cond ((null? sequence)
         initial)
        (else
         (operate (car sequence)
                  (accumulate operate initial (cdr sequence))))))
; accumulate-n 的实现
(define (accumulate-n operate initial sequence)
  (cond ((null? (car sequence))
         null)
        (else
         (cons (accumulate operate initial (map car sequence))
               (accumulate-n operate initial (map cdr sequence))))))