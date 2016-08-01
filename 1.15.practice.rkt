#lang racket
(define (cube x)
  (* x x x))

(define (n x)
  (- (* 3 x)
     (* 4 (cube x))))

(define (sine x)
  (if (< (abs x) 0.1)
      x
      (n (sine (/ x 3)))))
