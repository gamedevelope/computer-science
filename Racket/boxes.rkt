#lang racket

(define b (box "apple"))
(unbox b)
(set-box! b '(banana boat))
(set-box! b '(a b c d e))
(eq? b 'a)
(define b1 (box '(a b c d e)))
(eq? b b1)
(equal? b b1)
