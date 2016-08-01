#lang racket
(define (cube x)
  (* x x x))
;;另一种更精确的方法求intgral
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))
;两个辅助函数
(define (even? n)
  (= (remainder n 2) 0))
(define (odd? n)
  (= (remainder n 2) 1))
;主函数
(define (intergral f a b n)
  ;定义y
  (define (y k)
    (f (+ a (* k h))))
  ;定义h
  (define h
    (/ (- b a) n))
  ;定义term
  (define (term k)
    (* (/ h 3) (factor k) (y k)))
  ;定义factor
  (define (factor k)
    (cond ((or (= k 0) (= k n)) 1)
          ((odd? k) 4)
          ((even? k) 2)))
  ;定义next
  (define (next k)
    (+ 1 k))
  ;主函数
  (sum term 0 next n))