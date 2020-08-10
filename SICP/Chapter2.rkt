#lang sicp

; 第二章
; 构建数据抽象

; 章节 2.1.1
; 有理数的算术运算

; 构建一个有理数
(define (make-rat n d)
  (list n d))
(define (numer rat)
  (car rat))
(define (denom rat)
  (cadr rat))

; 加法
(define (add-rat x y)
  (/ (+ (* (numer x) (denom y))
        (* (numer y) (denom x)))
     (* (denom x) (denom y))))

; 减法
(define (sub-rat x y)
  (/ (- (* (numer x) (denom y))
        (* (numer y) (denom x)))
     (* (denom x) (denom y))))

; 乘法
(define (mul-rat x y)
  (/ (* (numer x) (numer y))
     (* (denom x) (denom y))))

; 除法
(define (dev-rat x y)
  (/ (* (numer x) (denom y))
     (* (denom x) (numer y))))

