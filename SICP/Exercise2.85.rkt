#lang sicp
(#%require "Common.rkt")

(define (raise n)
  ((get 'raise 'number) n))
(define (drop n)
  ((get 'drop 'number) n))
(define (project n)
  ((get 'drop 'project) n))

(raise (raise (make-rational 1 1)))
(project (make-complex-from-real-imag 1 0))
;(raise (make-real 1))
;(apply-generic 'equ? (make-real 1) (make-real 2))
;(project (make-complex-from-real-imag 1 0))
;(define c1 (make-complex-from-real-imag 1 0))
;(define r1 (make-real 1))
;(drop (make-complex-from-real-imag 1 0))
