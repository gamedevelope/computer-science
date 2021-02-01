#lang racket

(do ((x 100)
     (f (lambda (x) (+ x 1))))
  ((> x 1)
   (do ((y 10))
     ((> y 1) (+ x y)))))
