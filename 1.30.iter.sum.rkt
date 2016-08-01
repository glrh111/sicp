#lang racket
;;将sum的递归过程变为迭代过程，更快速
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))
(define (term x)
  (/ 1.0 (* x (+ x 2))))
(define (next n)
  (+ n 4))