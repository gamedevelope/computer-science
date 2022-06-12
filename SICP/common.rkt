#lang racket

(provide (all-defined-out))

(define (abs x)
  (if (< x 0) (- x) x))

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (double x)
  (* x 2))

(define (halve x)
  (/ x 2))

(define (lambda-cost f)
  (define t1 (current-inexact-milliseconds))
  (define v (f))
  (define t2 (current-inexact-milliseconds))
  (fprintf (current-output-port)
           "~a cost [~a ms] \n"
           v
           (- t2 t1)))

;;; (base ^ exp) mod m
(define (expmod base exp m)
  (cond [(= exp 0) 1]
        [(even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m)]
        [else
         (remainder
          (* base (expmod base (- exp 1) m))
          m)]))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

;;; 判断a能不能整除b
(define (divides? a b)
  (= (remainder b a) 0))

(define (runtime) (current-milliseconds))