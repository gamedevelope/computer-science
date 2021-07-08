#lang racket

(define f
  (lambda (x)
    (let ([y 5])
      (+ x y))))

(define g
  (lambda (append)
    (define cons (append "ugly" "confusiong"))
      (let ([append 'this-was])
        (list append cons))))