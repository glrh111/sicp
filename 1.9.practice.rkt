#lang racket
(define (inc x)
  (+ x 1))
(define (dec x)
  (- x 1))

(define (+a a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

;;(+a 4 5)的手动追踪
(+a 4 5)
(inc (+ (dec 4) 5))
(inc (+ 3 5))
(inc 8)
9

(define (+b a b)
  (if (= a 0)
      b
      (+ (inc a) (dec b))))

;;(+b 4 5)的手动追踪，应用序的eval方式
(+b 4 5)
(+ (inc 4) (dec 5))
(+ 5 4)
9