#lang sicp

; 牛顿迭代法与 new-if
(define (sqrt x)
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter
         (improve guess x)
         x)))
  (define (good-enough? guess x)
    (< (abs (- (* guess guess) x)) 0.00001))
  (define (improve guess x)
    (/ (+ guess (/ x guess)) 2.0))
  (sqrt-iter 1.0 x))
(sqrt 5)

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(new-if (< 0 1) 1 2)

; new-if 的问题在于，如果解释器是正则序求值，会进入死循环
(define (new-sqrt x)
  (define (sqrt-iter guess x)
    (new-if (good-enough? guess x)
        guess
        (sqrt-iter
         (improve guess x)
         x)))
  (define (good-enough? guess x)
    (< (abs (- (* guess guess) x)) 0.00001))
  (define (improve guess x)
    (/ (+ guess (/ x guess)) 2.0))
  (sqrt-iter 1.0 x))
(new-sqrt 10)
                