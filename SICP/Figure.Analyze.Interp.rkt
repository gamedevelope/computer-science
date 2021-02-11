#lang sicp
(define (eval exp env)
  ((analyze exp) env))

