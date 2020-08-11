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

(new-section "练习 2.1")
; 定义更好的 make-rat
; 分子,分母都为负数,将其转换成正数
; 如果分母为负,分子为整,将分子转换负数,分母转为正数
(define (make-rat n d)
  (cond ((= d 0) (error "should not be zero"))
        (else
         (let ((nd (* n d)))
           (let ((n (if (< d 0) (- 0 n) n)))
             (let ((d (/ nd n))
                   (c (abs (gcd n d))))
               (cons (/ n c)
                     (/ d c))))))))
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

(newline)
(make-rat 1 -5)
(make-rat -1 -2)

(new-section "练习 2.2")
; 练习 2.2
(define (make-segment x1 y1 x2 y2)
  (cons (cons x1 y1)
        (cons x2 y2)))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (x-coord p)
  (car p))

(define (y-coord p)
  (cdr p))

(define (left-point-x p)
  (car (x-point p)))
(define (left-point-y p)
  (cdr (x-point p)))
(define (right-point-x p)
  (car (y-point p)))
(define (right-point-y p)
  (cdr (y-point p)))

(define (midpoint-segment p)
  (let ((p1 (x-point p))
        (p2 (y-point p)))
    (cons (/ (+ (x-coord p1) (x-coord p2)) 2.0)
          (/ (+ (y-coord p1) (y-coord p2)) 2.0))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define p (make-segment 1 1 10 3))
(x-point p)
(y-point p)
(print-point p)
(newline)
(midpoint-segment p)

(define make-rectangle make-segment)

; 计算矩形周长
(define (rectangle-perimeter p)
  (+ (* (- (right-point-x p) (left-point-x p))
        2)
     (* (- (right-point-y p) (left-point-y p))
        2)))

; 计算矩形面积
(define (rectangle-area p)
  (* (- (right-point-x p) (left-point-x p))
     (- (right-point-y p) (left-point-y p))))

(right-point-x p)
(left-point-x p)
(rectangle-perimeter p)
(rectangle-area p)

(define q (make-rectangle 1 2 3 4))
(rectangle-perimeter q)
(rectangle-area q)

(new-section "练习 2.3")
(define (make-rectangle-v2 x1 y1 length height)
  (make-segment x1 y1 (+ x1 length) (+ y1 height)))
(define mr1 (make-rectangle-v2 0 0 5 10))
(rectangle-perimeter mr1)
(rectangle-area mr1)
