#lang racket

(provide (all-defined-out))

(define (abs x)
  (if (< x 0) (- x) x))

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (double x)
  (* x 2))

(define (halve x)
  (/ x 2))

(define (lambda-cost f)
  (define t1 (current-inexact-milliseconds))
  (define v (f))
  (define t2 (current-inexact-milliseconds))
  (fprintf (current-output-port)
           "~a cost [~a ms] \n"
           v
           (- t2 t1)))