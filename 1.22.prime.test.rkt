#lang racket
;;基于Fermat test测试是否为素数，并输出时间
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

;;计算消耗的时间
(define (timed-prime-test n)
  ;(newline)
  ;(display n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
  (cond ((fast-prime? n 10)
         (newline)
         (display n)
         (report-time (- (current-inexact-milliseconds) start-time)))))

(define (report-time elapsed-time)
  (display " ***** ")
  (display elapsed-time))

;;检测区间内的奇数的素性
(define (odd? a)
  (= 1 (remainder a 2)))

(define (search-for-primes a b)
  (cond ((> a b)
         (newline)
         (display "检测结束"))
        ((odd? a) (timed-prime-test a)
                  (search-for-primes (+ a 2) b))
        (else (search-for-primes (+ a 1) b))))
        