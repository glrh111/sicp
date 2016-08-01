#lang racket
;;用递归法计算pascal三角
;;row=a,column=b,f(a,b)=f(a-1,b-1)+f(a-1,b); b=1, 1; b=a, 1;
(define (pascal a b)
  (if (or (= b 1) (= b a))
      1
      (+ (pascal (- a 1) (- b 1))
         (pascal (- a 1) b))))