#lang racket
(define x1 (list (list 2 3) (list 1 (list 4 6) 4)))
(define x2 (list 4 6 7 4 2 3 ))
(define x3 (list 1 3 2 4 5))

;;
(define (accumulate operate initial sequence)
  (cond ((null? sequence)
         initial)
        (else
         (operate (car sequence)
                  (accumulate operate initial (cdr sequence))))))

(define (map p sequence)
  (accumulate (lambda (x y)
                (cons (p x)
                      y))
              null sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y))
              0
              sequence))