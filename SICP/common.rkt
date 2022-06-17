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

(define (inc x)
  (+ x 1))

(define (dec x)
  (- x 1))

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

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (smallest-divisor n)
  (find-divisor n 2 0))

(define (find-divisor n test-divisor times)
  (define (next n)
    (if (= n 2) 3
        (+ n 2)))
  (cond ((> (square test-divisor) n)
         n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor) (+ 1 times)))))

;;; 判断a能不能整除b
(define (divides? a b)
  (= (remainder b a) 0))

(define (runtime) (current-milliseconds))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))