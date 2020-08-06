#lang sicp

(define (square x) (* x x))

(define (fast-expt p n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt p (/ n 2))))
        (else (* p (fast-expt p (- n 1))))))

(fast-expt 2 10)

(define (fast-expt-v2 b n)
  (define (iter-expt b n a)
    (cond ((= n 0) 1)
          ((= n 1) (* b a))
          ((even? n) (iter-expt (square b) (/ n 2) a))
          (else (iter-expt b (- n 1) (* a b)))))
  (iter-expt b n 1))

(fast-expt-v2 2 10)