#lang racket
;; 利用连分式计算e自然底数
(define (cont-frac n d k)
  (define (iter product count)
    (if (= count 0)
        product
        (iter (/ (n count) (+ (d count) product)) (- count 1))))
  (iter (/ (n k) (d k)) (- k 1)))

; n = 1; d = 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8
(define (n x)
  1.0)
(define (d x)
  (cond ((= (remainder (+ x 1) 3) 0)
         (* 2 (/ (+ x 1) 3)))
        (else
         1)))

; 计算自然对数
(cont-frac n d 100)
