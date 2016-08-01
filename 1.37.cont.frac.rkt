#lang racket
;;定义有穷连分式：k未层数，n为分子，d为分母
(define (cont-frac n d k)
  (if (= k 1)
      (/ (n k) (d k))
      (/ (n k) (+ (d k) (cont-frac n d (- k 1))))))
(define (n x) 1.0)
(define (d x) 1.0)

;;迭代版本的
(define (cont-frac2 n d k)
  (define (iter product count)
    (if (= count 0)
        product
        (iter (/ (n count) (+ (d count) product)) (- count 1))))
  (iter (/ (n k) (d k)) (- k 1)))


;;计算4位精度需要多少步的
(define (cont-frac3 n d)
  (define (iter product count)
    (cond ((close-enough? (/ 1.0 product))
           (newline)
           (display count)
           (display ":")
           (display product))
           (else
            (iter (/ (n count) (+ (d count) product)) (+ count 1)))))
    (iter 1 0))

(define phi 1.618033988749895)
(define (close-enough? x)
  (< (abs (- x phi)) 0.00001))