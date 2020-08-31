#lang sicp
(define (element-of-set? x set)
  (cond ((null?  set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else
         (element-of-set? x (cdr set)))))

(element-of-set? 1 '(2 3 4))
(element-of-set? 4 '(2 3 4 5 6))

(define (intersection-set s1 s2)
  (if (or (null? s1) (null? s2))
      '()
      (let ((x1 (car s1))
            (x2 (car s2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr s1)
                                          (cdr s2))))
              ((< x1 x2)
               (intersection-set (cdr s1) s2))
              ((< x2 x1)
               (intersection-set s1 (cdr s2)))))))

(intersection-set '(1 2 3 4) '(3 4 5 6))