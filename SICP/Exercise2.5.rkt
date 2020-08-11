#lang sicp

(define (pow a b)
  (if (= b 1)
      a
      (* a (pow a (dec b)))))

(define (log a b)
  (if (> a b)
      0
      (+ 1 (log a (/ b a)))))

(define (cons x y)
  (* (pow 2 x) (pow 3 y)))

(define (car z)
  (if (= (remainder z 3) 0)
      (car (/ z 3))
      (log 2 z)))

(define (cdr z)
  (if (= (remainder z 2) 0)
      (cdr (/ z 2))
      (log 3 z)))
(define p (cons 2 3))
(car p)
(cdr p)
