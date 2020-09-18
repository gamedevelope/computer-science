#lang sicp
(#%require "Common.rkt")
; 3.1 累加器
(define (make-accumulator n)
  (lambda (x)
    (begin (set! n (+ n x))
           n)))

(define A (make-accumulator 0))
(define B (make-accumulator 10))

; 3.2 函数计数器
(define (make-monitored f)
  (let ((count 0))
    (lambda (input)
      (cond ((eq? input 'how-many-calls?) count)
            ((eq? input 'reset-count) (begin (set! count 0) true))
            (else
             (if (number? input)
                 (begin (set! count (inc count))
                        (f input))
                 (error "PARAM MUST BENUBER --" input)))))))
(let ((s (make-monitored sqrt)))
  (echo (s 100))
  (echo (s 10000))
  (echo (s 'how-many-calls?))
  (echo (s 'reset-count))
  (echo (s 100))
  (echo (s 10000))
  (echo (s 'how-many-calls?))
  (echo (s 'reset-count)))
