#lang sicp

(define (abs-plus a b)
  ((if (> b 0) + -) a b))