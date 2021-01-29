#lang racket

(cond [(member 5 '(0 1 2 3 4)) => (lambda (l) (map - l))]
      [(member 2 '(0 1 2 3 4)) => (lambda (l) (map - l))])

(define f (lambda (l) (map - l)))
(f (list 1 2 3))
(member 1 '(0 1 2 3 4))
(member 5 '(0 1 2 3 4))

(cond [(list 1 2 3) => (lambda (l) (map - l))])
