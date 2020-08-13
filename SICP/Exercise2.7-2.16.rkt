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

;(define (mul-interval x y)
;  (let ((p2 (* (lower-bound x) (upper-bound y)))
;        (p3 (* (upper-bound x) (lower-bound y))))
;    (make-interval (min p2 p3)
;                   (max p2 p3))))
(mul-interval n1 n2)

; 除法
(define (div-interval x y)
  (let ((a (upper-bound y))
        (b (lower-bound y)))
    (if (and (<= b 0) (<= 0 a))
        (error "除以跨0的区间")
        (mul-interval x
                      (make-interval (/ 1.0 (upper-bound y))
                                     (/ 1.0 (lower-bound y)))))))

(div-interval n1 n2)
(div-interval n1
              (make-interval -1 -0.00001))
; 练习 2.9
; n1与n2之和的宽度是n1与n2的宽度之和
; n1与n2之差的宽度是n1与n2的宽度之和

; 练习 2.11
; 分别比较两个区间与0的关系
;(define (mul-interval x y)
;  (let ((xl (lower-bound x))
;        (xu (upper-bound x))
;        (yl (lower-bound y))
;        (yu (upper-bound y)))
;    (cond ((< 0 xl)
;           (cond ((< 0 yl) (make-interval (* xl yl) (* xu yu)))
;                 ((< yl 0) (make-interval (* xu yl) (* xl yu)))
;                 (else (make-interval (* xu yl) (* xu yu)))))
;          ((< xu 0)
;           (cond ((< 0 yl) (make-interval (* xl yu) (* xu yl)))
;                 ((< yu 0) (make-interval (* xl yl) (* xu yu)))
;                 (else (make-interval (* xl yu) (* xl yl)))))
;          (else

; 练习 2.12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-interval-by-percent c p)
  (let ((d (/ (* p c) 100.0)))
    (make-center-width c (abs d))))

(define (percent i)
  (* 100 (/ (width i) (center i))))

(percent (make-interval-by-percent 10 10))

; 练习 2.13
; 误差很小时
; 以区间都是正整数为例，区间积的误差是
(define (mul-tolerance n1 n2)
  (percent (mul-interval n1 n2)))

; 误差之积是
(define (mul-of-tolerance x y)
  (* (percent x) (percent y)))

(define n3 (make-interval 9 10))
(define n4 (make-interval 99 800))

(display n3)
(display n4)
(newline)
(display "积的误差是")
(mul-tolerance n3 n4)
(newline)
(display "误差的积是")
(mul-of-tolerance n3 n4)
(newline)

(define n5 (make-interval 99 100))
(define n6 (make-interval 99 100))
(display n5)
(display n6)
(newline)
(display "积的误差是")
(mul-tolerance n5 n6)
(newline)
(display "误差的积是")
(mul-of-tolerance n5 n6)

; 练习 2.14
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define r1 (make-interval 3 5))
(define r2 (make-interval 4 6))
(par1 r1 r2)
(par2 r1 r2)

(div-interval r1 r1)
(div-interval r1 r2)

(set! r1 (make-interval-by-percent 10 20))
(set! r2 (make-interval-by-percent 10 20))
(par1 r1 r2)
(par2 r1 r2)

; 误差越大，两种计算方法的结果相差越大
(div-interval (make-interval 1 1)
              (div-interval (make-interval 1 1)
                            (make-interval 2 5)))

; r1 / r2
(div-interval r1 r2)

; r1 * 1 / r2
(mul-interval r1
              (div-interval (make-interval 1 1)
                            r2))

(set! r1 (make-interval-by-percent 10 10))
(set! r2 (make-interval-by-percent 10 10))
(define r3 (add-interval r1 r2))
(display r3)
(percent r3)
(width r3)
(center r3)
(define r4 (mul-interval r1 r2))
(display r4)
(percent r4)
; 练习 2.16
; 用如下的计算方法，可以消除两种计算方法的不同
; 但是乘积的误差计算不对
;(define (mul-interval x y)
;  (let ((p2 (* (lower-bound x) (upper-bound y)))
;        (p3 (* (upper-bound x) (lower-bound y))))
;    (make-interval (min p2 p3)
;                   (max p2 p3))))