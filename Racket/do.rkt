#lang racket
(do ((a 1)
     (b 2 (+ b 1)))
  ((> b 10) (+ a b))
  (begin (set! a (+ a 1))
         (set! a (* a 10))))

(do ((a 1)
     (b 2))
  ((> b 1) b))
