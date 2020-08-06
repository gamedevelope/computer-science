#lang sicp

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (fast-mul a b)
  (define (iter-mul a b result)
    (cond ((= 1 b) (+ a result))
          ((even? b) (iter-mul (double a) (halve b) result))
          (else (iter-mul a (- b 1) (+ result a)))))
  (iter-mul a b 0))