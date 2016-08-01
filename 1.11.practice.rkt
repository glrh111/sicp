#lang racket
;;两种方法时间复杂度差异惊人
;;迭代法计算f(n)=f(n-1)+2f(n-2)+3f(n-3)
(define (fn n)
  (define (fn-iter a b c count)
    (if (= count 0)
        c
        (fn-iter b c (+ (* a 3) (* b 2) c) (- count 1))))
  (if (< n 3)
      n
      (fn-iter 0 1 2 (- n 2)))) ;; 小子，注意啊，从初始值开始

;; 递归法计算
(define (fn2 n)
  (if (< n 3)
      n
      (+
       (fn2 (- n 1))
       (* 2 (fn2 (- n 2)))
       (* 3 (fn2 (- n 3))))))