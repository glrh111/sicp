#lang racket

;; 定义一些测试数据
(define x (cons 'a 'b))
(define y (cons 'c 'd))

; append! x y
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))