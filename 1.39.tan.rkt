#lang racket
;;迭代版本的
(define (cont-frac2 n d k)
  (define (iter product count)
    (if (= count 0)
        product
        (iter (/ (n count) (+ (d count) product)) (- count 1))))
  (iter (/ (n k) (d k)) (- k 1)))
;; 计算正切函数
(define (tan-cf x k)
  (define (n 