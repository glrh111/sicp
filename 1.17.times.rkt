#lang racket
;;迭代算法，只有加法和减法，定义惩罚，具有对数时间复杂度
(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (* a b)
  (*-iter a 0 b))

(define (*-iter a product b)
  (if (or (= b 0) (< b 0))
      product
      (if (even? b)
          (*-iter a (+ product (double a)) (- b 2))
          (*-iter a (+ product a) (- b 1)))))

