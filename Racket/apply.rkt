#lang racket

(define (avg lst)
  (/ (apply + lst) (length lst)))

(apply + '(1 2 3 4 5))

(apply - 0 '(1 2 3 4 5))

(apply * '(1 2 3 4 5))

(apply < '(1 2 3 4 5))

(+ 1 2 3 4 5)
