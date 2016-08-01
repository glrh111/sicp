#lang racket
;;使用最一般的方法检测是否为素数
;求平方
(define (square n)
  (* n n))
;检测是否整除
(define (divides? a b)
  (= 0 (remainder a b)))
;寻找能整除的数，从start开始寻找
(define (find-divisor n start)
  (cond ((> (square start) n) n)
        ((divides? n start) start)
        (else
         (find-divisor n (+ 1 start)))))
;上述函数的改进版本
(define (find-divisor2 n start)
  (define (next n)
    (if (= n 2)
        3
        (+ n 2)))
  (cond ((> (square start) n) n)
        ((divides? n start) start)
        (else
         (find-divisor2 n (next start)))))
;从2开始寻找的最小的除数
(define (smallest-divisor n)
  (find-divisor n 2))
(define (smallest-divisor2 n)
  (find-divisor n 2))
;检测是否为素数
(define (prime? n)
  (= n (smallest-divisor n)))
(define (prime2? n)
  (= n (smallest-divisor2 n)))

;;测量时间的工具