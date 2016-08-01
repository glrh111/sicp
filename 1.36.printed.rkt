#lang racket
(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  (define count 0)
  ;定义辅助函数
  (define (try-next now count)
    (let ((next (f now)))
      (cond ((close-enough? next now)
             (display "已经找到不动点:")
             (display next))
          (else
           (newline)
           (display count)
           (display ":")
           (display now)
           (try-next next (+ count 1))))))
  ;未辅助函数设置参数
  (try-next first-guess 0))
;计算x^x = 1000, 利用x |-> log(1000) / log(x)
(fixed-point (lambda (x) (/ (log 1000.0) (log x))) 2)
;采用平均阻尼计算的
(newline)
(fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 2)