#lang racket
;; (square-tree tree)
(define x1 (list 12 3 2 (list 4 2 (list 4 2) 2) (list 3 4)))

(define (square x)
  (* x x))

(define (square-tree tree)
  (cond ((null? tree)
         null)
        ((not (pair? tree))
         (square tree))
        ((cons (square-tree (car tree))
               (square-tree (cdr tree))))))
        
(define (square-tree2 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree2 sub-tree)
             (square sub-tree)))
       tree))
        