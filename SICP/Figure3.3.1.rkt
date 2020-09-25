#lang sicp

; 变动的表结构
; 练习 3.12
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

; 练习 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)