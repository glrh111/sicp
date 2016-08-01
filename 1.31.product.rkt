#lang racket
;;定义计算连×的高阶函数
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))
;基于以上高阶函数定义的阶乘函数
(define (factorial n)
  (define (term x)
    x)
  (define (next x)
    (+ 1 x))
  (product term 1 next n))
;基于以上高阶函数定义的`pi`/4 的近似运算
(define (square n)
  (* n n))
(define (time-pi n)
  (define (term a)
    (/ (* 1.0 (- a 1) (+ a 1)) (square a)))
  (define (next x)
    (+ x 2))
  (* 4 (product term 3 next n)))
      