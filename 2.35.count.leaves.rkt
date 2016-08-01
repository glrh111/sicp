#lang racket

(define x1 (list (list 2 3) (list 1 (list 4 6) 4)))
(define x2 (list 4 6 7 4 2 3 ))
(define x3 (list 1 3 2 4 5))


;;使用accumulate 重新定义count-leaves
(define (accumulate operate initial sequence)
  (cond ((null? sequence)
         initial)
        (else
         (operate (car sequence)
                  (accumulate operate initial (cdr sequence))))))

(define (count-leaves tree)
  (cond ((null? tree)
         0)
        ((not (pair? tree))
         1)
        (else
         (+ (count-leaves (car tree))
            (count-leaves (cdr tree))))))

(define (count-leaves2 tree)
  (map (lambda (sub-tree)
         (cond ((null? sub-tree)
                0)
               ((not (pair? sub-tree))
                1)
               (else
                (count-leaves2 sub-tree))))
       tree))
         

;;(define (count-leaves tree))
