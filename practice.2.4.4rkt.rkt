#lang racket
;; some assistant funcs
(define (square x)
  (* x x))
(define (attach tag contents)
  (cons tag contents))
(define (type-tag daum)
  (car daum))

;; 基础运算包
;; rectangular
(define (install-rectangular-package)
  
  ;; internal procedures
  (define (real-part z)
    (car z))
  (define (imag-part z)
    (cdr z))
  (define (make-from-real-imag x y)
    (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* (cos a) r)
          (* (sin a) r)))

  ;; interface to the rest of the system
  (define (tag x)
    (attach 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag '(rectangular)
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang '(rectangular)
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)

  ;; internal procedures
  (define (real-part z)
    (* (magnitude z)
       (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z)
       (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  (define (magnitude z)
    (car z))
  (define (angle z)
    (cdr z))
  (define (make-from-mag-ang r a)
    (cos r a))

  ;; interface to the rest of the system
  (define (tag z)
    (attach-tag 'polar z))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag '(polar)
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang '(polar)
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  'done)
  
;; apply-generic op . args 从表中取得操作符
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "no operates for those types:"
           (list op type-tags))))))

;; 选择函数
(define (real-part z)
  (apply-generic 'real-part z))
(define (imag-part z)
  (apply-generic 'imag-part z))
(define (magnitude z)
  (apply-generic 'magnitude z))
(define (angle z)
  (apply-generic 'angle z))
;; 构造函数
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

; 定义运算
; add-comlex z1 z2
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))
; sub-complex z1 z2
(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))
; mul-complex z1 z2
(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))
; div-complex z1 z2
(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

  
;; 测试数据
(define x (make-from-mag-ang 2 3.1))
(define y (make-from-mag-ang 2 3.1))