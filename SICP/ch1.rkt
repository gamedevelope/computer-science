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
  (define (cube-root-iter guess x)
    (if (good-enough? guess x)
        guess
        (cube-root-iter (improve guess x) x)))
  
  (define (good-enough? guess x)
    (< (abs (- (cube guess) x)) 0.001))
  
  (define (improve y x)
    (/ (+ (/ x (square y))
          (* 2 y))
       3.0))
  
  (define (cube-root x)
    (cube-root-iter 1 x))

  (println (cube-root 3))
  (println (cube (cube-root 3))))
