#lang racket

(require "common.rkt")

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))
(define (make-rat n d)
  (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (ex2.1)
  (define (make-rat n d)
    (let ((g (gcd n d))
          (p (* n d)))
      (let ((q (/ p (square g)))
            (m (abs (/ d g))))
        (cons (/ q m) m))))
  (println (make-rat 1 -2))
  (println (make-rat 2 -2))
  (println (make-rat 2 -4))
  (println (make-rat -2 -4))
  (println (make-rat 0 -4))
  )
(link 'ex2.1 ex2.1)

(define (ex2.2)
  (define (make-segment p1 p2)
    (cons p1 p2))
  (define (start-segment s)
    (car s))
  (define (end-segment s)
    (cdr s))
  (define (make-point x y)
    (cons x y))
  (define (x-point p)
    (car p))
  (define (y-point p)
    (cdr p))
  (define (midpoint-segment s)
    (let ((p1 (start-segment s))
          (p2 (end-segment s)))
      (make-point (/ (+ (x-point p1) (x-point p2)) 2)
                  (/ (+ (y-point p1) (y-point p2)) 2))))
  (define (print-point p)
    (newline)
    (display "(")
    (display (x-point p))
    (display ",")
    (display (y-point p))
    (display ")"))
  (define (print-segment s)
    (display "[")
    (print-point (start-segment s))
    (display ",")
    (print-point (end-segment s))
    (display "]"))
  (define s (make-segment (make-point 1 2)
                          (make-point 3 4)))
  (print-segment s)
  (print-point (midpoint-segment s))
  )
(link 'ex2.2 ex2.2)
(last-exercise)