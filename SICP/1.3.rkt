#lang sicp

; 1.3
; 定义一个过程，以三个数为参数，返回其中较大的两数之和
(define (sum-top-two a b c)
  (define (min a b)
    (cond ((< a b) a)
          (else b)))
  (- (+ a b c) (min a (min b c))))

