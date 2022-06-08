#lang racket

(#%provide abs
           cube
           println
           square
           )

(define (abs x)
  (if (< x 0) (- x) x))

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (println p)
  (display p)
  (newline))