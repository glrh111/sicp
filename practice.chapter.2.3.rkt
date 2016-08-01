#lang racket
;; chapter 2.3 是讲符号求值的
(define a 1)
(define b 2)
;; 该函数判断一个符号是否在一个表里边，返回#f，或符号第一次出现的子表(剩下的表)
(define (memq symbol items)
  (cond ((null? items)
         #f)
        ((eq? symbol (car items))
         items)
        (else
         (memq symbol (cdr items)))))

;; 定义两个测试数据
(define test1 '(x y (apple banana) z y apple hhh))
(define test2 '(wangli nihao woyehao))

;;; 符号求导系统

;; 选择一组选择函数和构造函数
; (variable? exp) 是否为变量
(define (variable? exp)
  (symbol? exp))
; (same-variable? v1 v2) 是否为相同变量
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

; (sum? e) 是否为∑
(define (sum? e)
  (and (pair? e) (eq? '+ (car e))))  
; (addend e) 被加数 (+ a b 中的a)
(define (addend e)
  (cadr e))
; (augend e) b
(define (augend e)
  (caddr e))
; (make-sum a1 a2)  构造加法式
(define (=number? v n)
  (and (number? v) (= v n)))
(define (make-sum a1 . a2)
  (cond ((null? a2)
         a1)
        ((=number? a1 0)
         (make-sum a2))
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        ((not (null? (cdr a2)))
         (make-sum a1 (make-sum (car a2) . (cdr a2))))
        (else
         (list '+ a1 a2))))

; (product? e) 是否为×式
(define (product? e)
  (and (pair? e) (eq? '* (car e))))
; (multiplier e) (* a b)的a
(define (multiplier e)
  (cadr e))
; (multiplicand e) (* a b)的b
(define (multiplicand e)
  (caddr e))
; (make-product m1 m2)  构造乘式
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0))
         0)
        ((=number? m1 1)
         m2)
        ((=number? m2 1)
         m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else
         (list '* m1 m2))))

; (exponentiation? e) 是否为幂运算？检测**
(define ** expt)
(define (exponentiation? e)
  (and (pair? e) (eq? '** (car e))))
; (base e) 幂的底数，第二个参数
(define (base e)
  (cadr e))
; (exponent e) 幂的指数，第三个参数
(define (exponent e)
  (caddr e))
; (make-exponentiation base exponent) 构造乘方
(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0)
         1)
        ((=number? exponent 1)
         base)
        ((variable? base)
         (list '** base exponent))
        (else
         (error "请正确输入幂"))))

; (deriv exp var)
(define (deriv exp var)
  (cond ((number? exp)
         0)
        ((variable? exp)
         (if (eq? exp var)
             1
             0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        
        ((product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv (multiplicand exp) var))
                   (make-product (multiplicand exp)
                                 (deriv (multiplier exp) var))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-exponentiation (base exp)
                                            (make-sum -1
                                                      (exponent exp)))))
        (else
         (error "错误的输入！"))))

; 测试数据
(define exp1 '(+ (* x (* x x)) (+ (* x x) 2)))
                       
