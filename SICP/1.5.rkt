#lang sicp

; 1.5
; 应用序求值，该程序能正常返回结果
; 正则序求值，该程序会陷入死循环
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))