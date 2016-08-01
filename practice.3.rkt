#lang racket
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "余额不足！！"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch op)
    (cond ((eq? 'withdraw op)
           withdraw)
          ((eq? 'deposit op)
           deposit)
          (else
           "错误的输入")))
  dispatch)

;; monte-carlo 求pi
(define (gcd m n)
  (cond ((= n 0)
         m)
        (else
         (gcd n (remainder m n)))))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 1))

