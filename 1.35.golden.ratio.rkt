#lang racket
(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  ;定义辅助函数
  (define (try-next now)
    (let ((next (f now)))
      (if (close-enough? next now)
          next
          (try-next next))))
  ;未辅助函数设置参数
  (try-next first-guess))
;利用上述函数，求黄金分割率 x |-> 1/x - 1
(fixed-point (lambda (x) (- (/ 1.0 x) 1)) 0.5)
;;为啥算出出来正值啊!!!!!