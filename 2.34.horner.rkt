#lang racket
;; Horner法计算多项式的结果
(define (accumulate operate initial sequence)
  (cond ((null? sequence)
         initial)
        (else
         (operate (car sequence)
                  (accumulate operate initial (cdr sequence))))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (now y)
                (+ now
                   (* x y)))
              0
              coefficient-sequence))