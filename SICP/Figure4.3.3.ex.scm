#lang sicp

(#%require "Figure4.3.3.rkt")

(define (succeed v next)
  'ok)

(define (fail v next)
  false)

(define (analyze input)
  (ambeval input genv succeed fail))

(analyze '(define (require p)
            (if (not p) (amb))))

(analyze '(define (aeo items)
            (require (pair? items))
            (amb (car items) (aeo (cdr items)))))

(analyze '(define (two-sum s list1 list2)
            (let ((a (aeo list1)))
              (let ((b (aeo list2)))
                (require (= s (+ a b)))
                (list a b)))))

;;; 练习 4.35
(analyze '(define (an-integer-between a b)
            (require (not (> a b)))
            (amb a (an-integer-between (+ a 1) b))))

(analyze '(define (a-pythagorean-triple-between low high)
            (let ((i (an-integer-between low high)))
              (let ((j (an-integer-between i high)))
                (let ((k (an-integer-between j high)))
                  (require (= (+ (* i i) (* j j)) (* k k)))
                  (list i j k))))))

;;; 练习 4.36

(driver-loop)