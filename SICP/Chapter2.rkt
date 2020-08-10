#lang sicp

; 第二章
; 构建数据抽象

; 章节 2.1.1
; 有理数的算术运算

; 构建一个有理数
(define (make-rat n d)
  (cons n d))
(define (numer rat)
  (car rat))
(define (denom rat)
  (cdr rat))

; 加法
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

; 减法
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

; 乘法
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

; 除法
(define (dev-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

; 打印
(define (print-rat p)
  (newline)
  (display (numer p))
  (display "/")
  (display (denom p)))

(define x1 (make-rat 5 3))
(define y1 (make-rat 1 5))

(add-rat x1 y1)
(sub-rat x1 y1)
(mul-rat x1 y1)
(dev-rat x1 y1)
(print-rat x1)
(print-rat (dev-rat x1 y1))