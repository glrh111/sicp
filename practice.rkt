#lang racket
(define (square x) ( * x x))

(define (sum-of-squares x y) (+
                              (square (+ x 1))
                              (square (* 4 y))))

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (abs1 x)
  (if (< x 0)
      (- x)
      x))

(define (>= x y)
  (not (< x y)))

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;; 计算平方根的
(define (sqrt-iter guess x)
  (if (good-enough2? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- x (* guess guess))) 0.0000000001))

(define (good-enough2? guess x)
  (define i-guess (improve guess x))
  (< (/ (abs (- guess i-guess)) i-guess) 0.000001)) 

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;; 计算立方根的
(define (cbrt-iter guess x)
  (if (cbrt-good-enough? guess x)
      guess
      (cbrt-iter (cbrt-improve guess x) x)))

(define (cbrt-improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (cbrt-good-enough? guess x)
  (define i-guess (cbrt-improve guess x))
  (< (/ (abs (- guess i-guess)) i-guess) 0.0000001))

(define (cbrt x)
  (cbrt-iter 1.0 x))

;;factorial 的一种实现方法
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

;;(factorial 6) 的手工追踪
(factorial 6)
(* 6 (factorial 5))
(* 6 (* 5 (factorial 4)))
(* 6 (* 5 (* 4 (factorial 3))))
(* 6 (* 5 (* 4 (* 3 (factorial 2)))))
(* 6 (* 5 (* 4 (* 3 (* 2 (factorial 1))))))
(* 6 (* 5 (* 4 (* 3 (* 2 1)))))
(* 6 (* 5 (* 4 (* 3 2))))
(* 6 (* 5 (* 4 6)))
(* 6 (* 5 24))
(* 6 120)
720

;;factorial 的另一种实现方法
(define (fact-iter product counter max-counter)
  (if (> counter max-counter)
      product
      (fact-iter (* product counter)
                 (+ 1 counter)
                 max-counter)))

(define (factorial2 n)
  (fact-iter 1 1 n))

;;(factorial2 6)的手工追踪
(factorial2 6)
(fact-iter 1 1 6)
(fact-iter 1 2 6)
(fact-iter 2 3 6)
(fact-iter 6 4 6)
(fact-iter 24 5 6)
(fact-iter 120 6 6)
(fact-iter 720 7 6)
720

;;factorial的简化版本
(define (factorial3 n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* product counter)
              (+ 1 counter))))
  (iter 1 1))

;;fabnacci的一种方法
(define (fabnacci n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fabnacci (- n 1))
                 (fabnacci (- n 2))))))
;;fabnacci的另一种方法
(define (fabnacci2 n)
  (define (fab-iter a b counter)
    (if (= counter 0)
        b
        (fab-iter b (+ a b) (- counter 1))))
  (fab-iter 0 1 (- n 1)))

;;换零钱的方式有多少种。需要参数：总金额amount，零钱种类及金额
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount (- kinds-of-coins 1))
                 (cc (- amount (first-coin-amount kinds-of-coins)) kinds-of-coins)))))

(define (first-coin-amount kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))


;;欧几里得法计算最大公约数 a > b
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;;寻找整数的最小整数yinzi
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (* test-divisor test-divisor) n) n)
        ((divides? n test-divisor) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder a b) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;; base 的 exp次方，divide m 的yushu
(define (even? n)
  (= (remainder n 2) 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (expmod (* base base) (/ exp 2) m) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

;take a random no check it!
(define (fermat-test n)
  (define (try-it a)
    (= a (expmod a n n)))
  (try-it (+ 1 (random (- n 1)))))

(define (prime2? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (prime2? n (- times 1)))
        (else #f)))

;;1.3 高阶函数的抽象
(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (cube1-3 n)
  (* n n n))
(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube1-3 a) (sum-cubes (+ a 1)b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))
;;抽象出来的一个求和模式
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (term a)
  (/ 1.0 (* a (+ a 2))))
(define (next a)
  (+ a 4))
;;计算定积分的
(define (int f a b dx)
  (define (add-dx a) (+ a dx))
  (* (sum f (+ a dx (/ dx 2)) add-dx b) dx))
;;初学let绑定局部变量
(define (cal-f x y)
  ((lambda (a b)
     (+ (* (square a) x)
        (* b y)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

;以上procedure等价于下述过程
(define (cal-f-let x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* (square a) x)
       (* b y)
       (* a b))))

;;1.33计算使f(x)=0成立的x
; 测试参数的正负
(define (positive? x)
  (> x 0))
(define (negtive? x)
  (< x 0))
(define (search f neg-point pos-point)
  ; 测试区间是否足够小
  (define (close-enough? a b)
    (< (abs (- b a)) 0.0001))

  ;区间足够小的话，返回中值mid-point
  (let ((mid-point (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        mid-point
        ;测试中值对应的函数值的正负
        (let ((mid-value (f mid-point)))
          (cond ((positive? mid-value)
                 (search f neg-point mid-point))
                ((negtive? mid-value)
                 (search f mid-point pos-point))
                (else
                 mid-point))))))
;定义一个更健全的函数，折半法求函数根
(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negtive? a-value) (positive? b-value))
           (search f a b))
          ((and (negtive? b-value) (positive? a-value))
           (search f b a))
          (else
           (display "输入错误！！！请重新设置参数")))))
;定义一个测试函数f(x) = x^3 - 2x - 3
(define (test x)
  (+ (* x x x) (* (- 2) x) (- 3)))


;;1.33 求不动点fixed-point
(define (fixed-point f first-guess)
  ;定义结束误差
  (define tolerance 0.0001)
  ;定义结束条件函数
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  ;主函数体
  (let ((next (f first-guess)))
    (if (close-enough? first-guess next)
        next
        (fixed-point f next))))
;基于以上函数定义开平方函数  y = (y + x / y) / 2
(define (fixed-sqrt x)
  (fixed-point (lambda (y) (/ (+ y (/ x y)) 2.0)) 1))
                