#lang racket

(require "common.rkt")
(require racket/trace)
(require rackunit)

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
  (println (fast-expt 2 100))
  )
(ex1.16)

;;; Exercise 1.17
(define (ex1.17)
  (define (fast-mul a b)
    (cond ((= a 0) 0)
          ((= b 0) 0)
          ((= b 1) a)
          ((= b -1) (- a))
          ((< a b) (fast-mul b a))
          ((even? b) (fast-mul (double a) (halve b)))
          (else
           (+ a (fast-mul a (- b 1))))))
  (check-equal? (fast-mul 10 10) (* 10 10))
  (println (fast-mul 10 10))
  (println (fast-mul 99 99))
  (println (fast-mul 2 -1024))
  )
(ex1.17)

;;; Exercise 1.18 跟 1.17 要求一样
;;; 换成迭代的计算方式
(define (ex1.18)
  (define (iter r a b)
      (cond [(= b 0) r]
            [(even? b)
             (iter r (double a) (halve b))]
            [else
             (iter (+ r a)
                   a
                   (- b 1))]))
  (define (fast-mul a b)
    (iter 0 a b))
  (check-equal? (fast-mul 10 10) (* 10 10))
  (check-equal? (fast-mul 0 0) (* 0 0))
  (check-equal? (fast-mul 1 1) (* 1 1))
  (println (fast-mul 1024 1023))
  )
(ex1.18)

;;; Exercise 1.19
;(define (ex1.19)
;  (define (fib n)
;    (fib-iter 1 0 0 1 n))
;  (define (fib-iter a b p q count)
;    (cond [(= count 0) b]
;          [(even? count)
;           (fib-iter a
;                     b
;                     ..
;                     ..
;                     (/ count 2))]
;          [else
;           (fib-iter (+ (* b q) (* a q) (* a p))
;                     (+ (* b p) (* a q))
;                     p
;                     q
;                     (- count 1))]))
;  (println (fib 10))
;  )
;(ex1.19)

; 1.2.6

(define (prime? n)
  (= n (smallest-divisor n)))

(define (ex1.21)
  (println (smallest-divisor 199))
  (println (smallest-divisor 1999))
  (println (smallest-divisor 19999))
  )
(ex1.21)

(define (ex1.22)
  (define (timed-prime-test n)
    (newline)
    (display n)
    (start-prime-test n (runtime)))

  (define (start-prime-test n start-time)
    (if (prime? n)
        (report-prime (- (runtime) start-time))
        0))
  
  (define (report-prime elapsed-time)
    (display " **** ")
    (display elapsed-time))

  (timed-prime-test 1125899839733759)
  )
(ex1.22)

;;;
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))