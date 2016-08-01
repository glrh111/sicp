#lang racket
;; 构造函数
; make-from-real-imag real-part imag-part
(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))
; make-from-mag-ang magnitude angle
(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))
; 构造的基础 z = ('type-tag . (complex))
; type-tag部分
; attach-tag type-tag contents
(define (attach-tag type-tag contents)
  (cons type-tag contents))
; type-tag datum
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "wrong input.")))
; contents datum
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "error input.")))
; rectangular? z
(define (rectangular? datum)
  (eq? 'rectangular (type-tag datum)))
; polar? z
(define (polar? datum)
  (eq? 'polar (type-tag datum)))
; 基于直角坐标定义的六个函数
(define (square x)
  (* x x))
(define (real-part-rectangular z)
  (car z))
(define (imag-part-rectangular z)
  (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))
(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))
(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'polar (cons (* r (cos a))
                           (* r (sin a)))))

; 基于极坐标定义的六个函数
(define (real-part-polar z)
  (* (magnitude-polar z)
     (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z)
     (sin (angle-polar z))))
(define (magnitude-polar z)
  (car z))
(define (angle-polar z)
  (cdr z))
(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))
(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar
              (cons r a)))

;; 选择函数
; real-part
(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else
         (error "error"))))
; imag-part
(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else
         (error "error"))))
; magnitude
(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else
         (error "error"))))
; angle = atan y x
(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else
         (error "error"))))
          


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