#lang racket
;;定义的一个更通用的结合函数
(define (accumulate combiner null-value term a next b)
  (define (iter start result)
    (if (> start b)
        result
        (iter (next start) (combiner start result))))
  (iter a null-value))
;基于以上函数定义sum
(define (sum term a next b)
  (accumulate + 0 term a next b))
;基于以上函数定义product
(define (product term a next b)
  (accumulate * 1 term a next b))

;;定义递归版的accumulate来
(define (accumulate2 combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate2 combiner null-value term (next a) next b))))

      