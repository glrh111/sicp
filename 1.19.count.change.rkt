#lang racket
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define us-coins (list 50 25 10 5 1))

(define (cc amount coins)
  (cond ((or (< amount 0) (null? coins))
         0)
        ((= amount 0)
         1)
        (else
         (+ (cc amount (except-first-denomination coins))
            (cc (- amount (first-denomination coins))
                coins)))))

(define (except-first-denomination list)
  (cdr list))

(define (first-denomination list)
  (car list))
         