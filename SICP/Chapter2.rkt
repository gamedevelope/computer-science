#lang sicp

; 公共方法
(define (gcd m n)
  (if (= n 0)
      m
      (gcd n (remainder m n))))

; 章节分割
(define (new-section msg)
  (newline)
  (newline)
  (display "* * * * * * *")
  (newline)
  (display msg)
  (newline)
  (display "* * * * * * *")
  (newline))

; 第二章
; 构建数据抽象

; 章节 2.1.1
; 有理数的算术运算

; 构建一个有理数
;(define (make-rat n d)
;  (cond ((= d 0) (error "should not be zero"))
;        (else
;         (let ((c (gcd n d)))
;           (cons (/ n c)
;                 (/ d c))))))

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

(define x1 (make-rat 3 10))
(define y1 (make-rat 2 5))

(add-rat x1 y1)
(sub-rat x1 y1)
(mul-rat x1 y1)
(dev-rat x1 y1)
(print-rat x1)
(print-rat (dev-rat x1 y1))

(new-section "练习 2.1")
; 定义更好的 make-rat
; 分子，分母都为负数，将其转换成正数
; 如果分母为负，分子为整，将分子转换负数，分母转为正数
(define (make-rat n d)
  (cond ((= d 0) (error "should not be zero"))
        (else
         (let ((c (gcd n d))
               (nd (* n d)))               
           (cons (/ (/ nd d) c)
                 (/ (/ nd  c))))))

(make-rat 1 -1)
(make-rat -1 -2)
