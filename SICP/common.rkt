#lang racket

(#%provide abs
           println
           square
           )

(define (abs x)
  (if (< x 0) (- x) x))

(define (square x)
  (* x x))

(define (println p)
  (display p)
  (newline))