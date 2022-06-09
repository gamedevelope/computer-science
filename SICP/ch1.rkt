#lang racket

(#%require "common.rkt")

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
  (println (A 1 10))
  (println (A 2 4))
  (println (A 3 3)))
(ex1.10)