#lang racket
;;定义基本操作
;构造分式
(define (make-rat n d)
  ;绑定gcd
  (let ((g (gcd (abs n) (abs d))))
    ;根据成绩判断正值还是负值
    (cond ((> (* n d) 0)
           (cons
            (/ (abs n) g)
            (/ (abs d) g)))
          ((< (* n d) 0)
           (cons
            (/ (- (abs n)) g)
            (/ (abs d) g))))))
;返回分子
(define (numer x)
  (car x))
;返回分母
(define (denom x)
  (cdr x))

;;定义四则运算
;加法+
(define (add-rat x y)
  (/ (+ (* (numer x) (denom y))
        (* (numer y) (denom x)))
     (* (denom x) (denom y))))
;减法-
(define (sub-rat x y)
  (/ (- (* (numer x) (denom y))
        (* (numer y) (denom x)))
     (* (denom x) (denom y))))
;乘法×
(define (mul-rat x y)
  (/ (* (numer x) (numer y))
     (* (denom x) (denom y))))
;除法÷
(define (div-rat x y)
  (/ (* (numer x) (denom y))
     (* (denom x) (numer y))))

;;是否相同？
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))

;;显示相关
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;;一些辅助函数
(define (gcd x y)
  (if (< x y)
      (gcd y x)
      (cond ((= y 0)
             x)
            (else
             (gcd y (remainder x y))))))


;; 关于表的一些东西
(define x '(1 2 3 45 5 4 3 2))
(define y '(56 4 5 67 4))
; 通过索引取值
(define (list-ref items index)
  (if (= index 0)
      (car items)
      (list-ref (cdr items)
                (- index 1))))
; 计算表的元素个数
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
; 迭代的方法实现计算元素个数的函数
(define (length2 items)
  (define (iter product count)
    (if (null? product)
        count
        (iter (cdr product)
              (+ count 1))))
  (iter items 0))
; 定义一个追加函数(append list1 list2)
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1)
            (append (cdr list1)
                    list2))))
; practice 2.17 (last-pair list)返回最后一个元素
(define (last-pair list)
  (if (null? (cdr list))
      (car list)
      (last-pair (cdr list))))

; reverse list 倒序元素
(define (reverse list)
  (if (null? (cdr list))
      list
      (append (reverse (cdr list)) (cons (car list) null))))

; same-parity
(define (same-parity x . y)
  (let ((y-len (length y))
        (parity (remainder x 2)))
    (define (iter product count)
      (cond ((< count 0)
             product)
            ((= parity (remainder (list-ref y count) 2))
             (iter (cons (list-ref y count) product) (- count 1)))
            (else
             (iter product (- count 1)))))
    (cons x (iter '() (- y-len 1)))))
(same-parity 1 2 3 5 7 9 2)

;; map 映射 map predicate items
(define (map predicate items)
  (if (null? items)
      null
      (cons (predicate (car items))
            (map predicate (cdr items)))))

; foreach predicate items  为items执行predicate
(define (for-each predicate items)
  (define (iter temp-list)
    (cond ((null? temp-list)
           (newline))
          (else
           (predicate (car temp-list))
           (iter (cdr temp-list)))))
  (iter items)) ;;;; nice!!!

; count-leaves tree
(define (count-leaves tree)
  (cond ((null? tree)
         0)
        ((not (pair? tree))
         1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))
(define tree (list (list 3 4)
                   (list 1 (list 2 3))))

; deep-reverse 将子树都翻转  *****************************堪称漂亮
(define (deep-reverse tree)
  (if (pair? tree)
      (reverse (map deep-reverse tree))
      tree))

(define (deep-reverse2 tree)
  (cond ((null? tree)
         null)
        ((not (pair? tree))
         tree)
        (else (append (deep-reverse2 (cdr tree))
                      (list (deep-reverse2 (car tree)))))))
         
         
(define x1 (list (list 1 2) 3 4))
(define x2 (list (list (list 1 2) 3 4) 4 6 7 5 (list 1 2)))

(define (fringe tree) ;*******************这个跳过，有难度
  (let ((result null))
    (define (temp tree2)
      (cond ((null? tree2)
             null)
            ((not (pair? tree2))
             (append result (list tree)))
            (else
             (temp (car tree2))
             (temp (cdr tree2)))))
    (temp tree)
    (display "hha")
    result))

(define (scale-tree tree factor)
  (cond ((null? tree)
         null)
        ((not (pair? tree))
         (* tree factor))
        (else
         (cons (scale-tree (car tree) factor)
               (scale-tree (cdr tree) factor)))))

(define (scale-tree2 tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree2 sub-tree factor)
             (* sub-tree factor)))
       tree))


;; APL的设计思想
;; 过滤器
(define (filter predicate sequence)
  (cond ((null? sequence)
         null)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else
         (filter predicate (cdr sequence)))))
;; 累加器
(define (accumulate operate initial sequence)
  (cond ((null? sequence)
         initial)
        (else
         (operate (car sequence)
                  (accumulate operate initial (cdr sequence))))))
;; 枚举器
(define (enumerate-tree tree)
  (cond ((null? tree)
         null)
        ((not (pair? tree))
         (list tree))
        (else
         (append (enumerate-tree (car tree))
                 (enumerate-tree (cdr tree))))))