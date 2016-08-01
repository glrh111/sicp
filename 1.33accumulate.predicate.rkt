#lang racket
;;定义一个带过滤的累运算器
(define (accumulate combiner predicate? null-value term a next b)
  (define (iter start result)
    (cond ((> start b) result)
          ((predicate? start)
           (iter (next start) (combiner start result)))
          (else
           (iter (next start) result))))
       
  (iter a null-value))

;基于以上函数，计算一定范围内的素数的和
(define (sum-primes a b)
  (define (term a)
    a)
  (define (next a)
    (+ a 1))
  (accumulate + prime? 0 term a next b))
;以下是素数过滤器
(define (even? n)
  (= (remainder n 2) 0))
(define (square n)
  (* n n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= a (expmod a n n)))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n count)
  (cond ((= count 0) #t)
        ((fermat-test n)
         (fast-prime? n (- count 1)))
        (else #f)))

(define (prime? n)
  (cond ((> n 2)
         (fast-prime? n 5))
        (else
         #t)))

;基于第一个函数，计算所有与n互素的比n小的数的乘积
(define (product-primes n)
  (define (term x)
    x)
  (define (next x)
    (+ x 1))
  (define (gcd-prime? x)
    (= 1 (gcd n x)))
  (accumulate * gcd-prime? 1 term 1 next n))
;以下是互质的判断
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(define (gcd-primes2? a b)
  ( = 1 (gcd a b)))
