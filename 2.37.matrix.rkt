#lang racket
(define (accumulate operate initial sequence)
  (cond ((null? sequence)
         initial)
        (else
         (operate (car sequence)
                  (accumulate operate initial (cdr sequence))))))

(define (accumulate-n operate initial sequence)
  (cond ((null? (car sequence))
         null)
        (else
         (cons (accumulate operate initial (map car sequence))
               (accumulate-n operate initial (map cdr sequence))))))

; v = (v_i)    vector
; m = (m_ij)   matrix
(define v1 (list 1 2 3 4 5))
(define v2 (list 2 3 4 5 6))
(define m1 (list (list 1 2 3 4 5)
                 (list 6 5 4 3 2)
                 (list 2 3 4 5 6)
                 (list 4 5 6 7 8)
                 (list 9 8 7 6 4)
                 (list 1 2 3 4 5)))
(define m2 (list (list 1 6 2 4 9 1)
                 (list 2 5 3 5 8 2)
                 (list 3 4 4 6 7 3)
                 (list 4 3 5 7 6 4)
                 (list 5 2 6 8 4 5)))

; 定义v1 dot v2 返回向量和矩阵的点乘
(define (dot-product vector1 vector2)
  (accumulate + 0 (map * vector1 vector2)))

; 定义M dot v1 返回矩阵和向量的乘积
(define (matrix-*-vector matrix vector)
  (map (lambda (sub-matrix)
         (dot-product vector sub-matrix))
       matrix))

; 定义矩阵的转置
(define (transpose matrix)
  (accumulate-n cons
                null
                matrix))

; 定义矩阵的乘法
(define (matrix-*-matrix matrix1 matrix2)
  (let ((cols (transpose matrix2)))
    (map (lambda (sub-matrix)
           (matrix-*-vector cols sub-matrix))
         matrix1)))
  