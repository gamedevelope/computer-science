#lang sicp

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f) (lambda (x) (f (f (f x))))))

; 定义加法
(define (+ p1 p2)
  (lambda (f) (lambda (x) ((p1 f) ((p2 f) x)))))

; 定义乘法
(define (* p1 p2)
  (lambda (f) (lambda (x) ((p1 (p2 f)) x))))

(define n9 (* three three))
((n9 inc) 1)

(define n18 (+ n9 n9))
((n18 inc) 1)

(define n81 (* n9 n9))
((n81 inc) 1)