#lang racket
(define s1 (list 2 3 4 5))
(define s2 (list 4 5 6 7 10))
; element-of-set? obj set
(define (element-of-set? item set)
  (cond ((null? set)
         #f)
        ((equal? item (car set))
         #t)
        (else
         (element-of-set? item (cdr set)))))
; adjoin-set item set - return item + set
(define (adjoin-set item set)
  (if (element-of-set? item set)
      set
      (cons item set)))
; intersection set1 set2 - intersection
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2))
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else
         (intersection-set (cdr set1) set2))))
; union-set set1 set2 - return union
(define (union-set set1 set2)
  (cond ((null? set1)
         set2)
        ((null? set2)
         set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else
         (cons (car set1)
               (union-set (cdr set1) set2)))))

;; 第二中方法sets as ordered lists
(define (element-of-set2? x set)
  (cond ((null? set)
         #f)
         ((= x (car set))
          #t)
         ((< x (car set))
          false)
         (else
          (element-of-set? x (cdr set)))))
; 定义intersection-set s1 s2
(define (intersection-set2 set1 set2)
  (cond ((or (null? set1) (null? set2))
         '())
        (else
         (let ((x1 (car set1))
               (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1
                        (intersection-set (cdr set1) (cdr set2))))
                 ((> x1 x2)
                  (intersection-set set1 (cdr set2)))
                 ((< x1 x2)
                  (intersection-set (cdr set1) set2)))))))
; 定义adjoin obj set
(define (adjoin item set)
  (cond ((element-of-set2? item set)
         set)
        (else
         (let ((x (car set)))
           (cond ((> item x)
                  (cons  x
                         (adjoin item (cdr set))))
                 ((< item x)
                  (cons item set)))))))
               

; 定义union-join set1 set2
(define (union-set2 set1 set2)
  (cond ((null? set1)
         set2)
        ((null? set2)
         set1)
        (else
         (let ((x1 (car set1))
               (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1
                        (union-set2 (cdr set1) (cdr set2))))
                 ((> x1 x2)
                  (cons x2
                        (union-set2 set1 (cdr set2))))
                 ((< x1 x2)
                  (cons x1
                        (union-set2 (cdr set1) set2))))))))

;; 定义数据结构，用二叉树binary tree表示
(define (entry tree)
  (car tree))
(define (left-branch tree)
  (cadr tree))
(define (right-branch tree)
  (caddr tree))
(define (make-tree entry left-branch right-branch)
  (list entry left-branch right-branch))
; 基于binary tree做上述操作v3.0
(define t3 (list 3
                 (list 1
                       (list 0.5 '() '())
                       (list 2 '() '()))
                 (list 4
                       (list 3 '() '())
                       (list 5 '() '()))))

(define (element-of-set3? item set)
  (cond ((null? set)
         #f)
        ((= item (entry set))
         #t)
        ((> item (entry set))
         (element-of-set3? item (right-branch set)))
        ((< item (entry set))
         (element-of-set3? item (left-branch set)))))

(define (adjoin3 item set)
  (cond ((null? set)
         (make-tree item '() '()))
        ((= item (entry set))
         set)
        ((> item (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin3 item (right-branch set))))
        ((< item (entry set))
         (make-tree (entry set)
                    (adjoin3 item (left-branch set))
                    (right-branch set)))))
         

               