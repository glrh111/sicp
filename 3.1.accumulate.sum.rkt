#lang racket
;; 累加器
(define (make-accumulator sum)
  (lambda (x)
    (set! sum (+ sum x))
    sum))