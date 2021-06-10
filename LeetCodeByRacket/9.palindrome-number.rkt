#lang racket

(define/contract (is-palindrome x)
  (-> exact-integer? boolean?)
  (let ((s (string->list (number->string x))))
    (equal? s (reverse s)))
  )