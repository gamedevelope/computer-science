#lang sicp

; 练习 2.7
(define (make-interval a b)
  (cons a b))

(define (lower-bound x)
  (let ((a (car x))
        (b (cdr x)))
    (if (< a b) a b)))

(define (upper-bound x)
  (let ((a (car x))
        (b (cdr x)))
    (if (> a b) a b)))

(define n1 (make-interval 1 2))
(define n2 (make-interval 3 4))
(display n1)
(newline)
(display n2)
(newline)

; 加法
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(add-interval n1 n2)

; 减法 练习 2.8
(define (sub-interval n1 n2)
  (make-interval (- (lower-bound n1) (upper-bound n2))
                 (- (upper-bound n1) (lower-bound n2))))

(sub-interval n1 n2)

; 乘法
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(mul-interval n1 n2)

; 除法
(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(div-interval n1 n2)

; 练习 2.9