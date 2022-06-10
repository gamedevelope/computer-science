#lang racket

(require "common.rkt")
(require racket/trace)

;;; Exercise 1.3
(define (ex1.3)
  (define (f a b c)
    (if (and (<= a b) (<= a c))
        (+ (square b) (square c))
        (f b c a)))

  (println (f 1 2 3))
  (println (f 4 3 2)))

;;; Exercise 1.8
(define (ex1.8)
  (define (cube-root x)
    (define (cube-root-iter guess)
      (if (good-enough? guess)
          guess
          (cube-root-iter (improve guess))))
  
    (define (good-enough? guess)
      (< (abs (- (cube guess) x)) 0.001))
  
    (define (improve y)
      (/ (+ (/ x (square y))
            (* 2 y))
         3.0))
    (cube-root-iter 1))

  (println (cube-root 3))
  (println (cube (cube-root 3)))

  (println (cube-root -10))
  (println (cube (cube-root -10))))

;;; Exercise 1.10
(define (ex1.10)
  (define (A x y)
    (cond ((= y 0) 0)
          ((= x 0) (* 2 y))
          ((= y 1) 2)
          (else (A (- x 1) (A x (- y 1))))))
  (println (A 1 10)) ; 1024
  (println (A 2 4))  ; 65536
  (println (A 3 3))  ; 65536

  (define (f n) (A 0 n)) ; (define (f n) (* 2 n))
  (println (f 11))
  
  (define (g n) (A 1 n)) ; (define (g n) (expt 2 n))
  (println (g 11))

  (define (h n) (A 2 n))
  ;  (define (h n)
  ;    (if (<= n 1)
  ;        2
  ;        (expt 2 (h (- n 1)))))
  ; (h n) => 2 ^ 2 ^ 2 ... ^ 2 count 2 is n
  (println (h 4))
  )
(ex1.10)

;;; Exercise 1.11
;;; 递归版本
(define (ex1.11-v1)
  (define (f n)
    (if (< n 3)
        n
        (+ (f (- n 1))
           (* 2 (f (- n 2)))
           (* 3 (f (- n 3))))))
  (lambda-cost (lambda ()(f 15))))
(ex1.11-v1)
;;; 迭代版本
(define (ex1.11-v2)
  (define (f n)
    (define (iter x v1 v2 v3)
      (if (<= n x)
          v1
          (iter (+ x 1)
                (+ v1 (* 2 v2) (* 3 v3))
                v1
                v2)))
    (if (< n 3)
        n
        (iter 2 2 1 0)))
  (println (f 1))
  (println (f 2))
  (println (f 3))
  (lambda-cost (lambda ()(f 15)))
  )
(ex1.11-v2)

;;; Exercise 1.12
;;; 帕斯卡三角形
;;; 由于本章还没有开始介绍 cons, car, cdr 等操作
;;; 所以暂时只提供 (f x y) 来表示第几行第几个数
(define (ex1.12)
  (define (f x y)
    (cond ((< x y) (error "x must <= y"))
          ((= y 1) 1)
          ((= x y) 1)
          (else
           (+ (f (- x 1) (- y 1))
              (f (- x 1) y)))))

  (println (list (f 4 1) (f 4 2) (f 4 3) (f 4 4)))
  (println (list (f 5 1) (f 5 2) (f 5 3) (f 5 4) (f 5 5))))
(ex1.12)

;;; Exercise 1.13
;;; 根据提示很容易证明结论

;;; Exercise 1.14

;;; Exercise 1.16
(define (ex1.16)
  ;;; 使用迭代的方式求幂
  ;;; n不能是负整数
  (define (iter p x n)
    (cond ((= n 0) 1)
          ((= n 1) p)
          ((odd? n) (iter (* p x) x (- n 1)))
          (else
           (iter (* p p) x (/ n 2)))))
  (define (fast-expt x n)
    (iter x x n))
  (println (fast-expt 1 4))
  (println (fast-expt 2 0))
  (println (fast-expt 2 4))
  (trace iter)
  (println (fast-expt 2 100))
  )
(ex1.16)