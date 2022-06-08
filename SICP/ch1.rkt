#lang racket

(#%require "common.rkt")

; Exercise 1.3: Define a procedure that takes
; three numbers as arguments and returns the
; sum of the squares of the two larger numbers.

(define (ex1.3)
  (define (f a b c)
    (if (and (<= a b) (<= a c))
        (+ (square b) (square c))
        (f b c a)))

  (println (f 1 2 3))
  (println (f 4 3 2)))