#lang racket
;;fast-expt 使用对数次步骤计算幂
(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt base n)
  (fast-expt-iter base  1 n))

(define (fast-expt-iter base product count)
  (if (or (= count 0) (< count 0))
      product
      (if (even? count)
          (fast-expt-iter base (* product base base) (- count 2))
          (fast-expt-iter base (* product base) (- count 1)))))
            