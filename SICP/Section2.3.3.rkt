#lang sicp

; 2.3.3 实例：集合的镖师
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(element-of-set? 1 '(1 2 3 4 5))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(adjoin-set 1 '(2 3 4 5))

(define (intersection-set s1 s2)
  (cond ((or (null? s1) (null? s2)) '())
        ((element-of-set? (car s1) s2)
         (cons (car s1)
               (intersection-set (cdr s1)
                                 s2)))
        (else
         (intersection-set (cdr s1) s2))))

; 练习 2.59
(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        ((element-of-set? (car s1) s2)
         (union-set (cdr s1) s2))
        (else
         (cons (car s1)
               (union-set (cdr s1)
                          s2)))))

; 练习 2.60
; 允许集合中的元素重复
; 1.如果加入集合的元素一般都是不重复的，可以采用这种方法
(define (element-of-set-v2? x s)
  (cond ((null? s) false)
        ((= (car s) x) true)
        (else
         (element-of-set-v2? x (cdr s)))))

(define (adjoin-set-v2 x s)
  (cons x s))

(define (intersection-set-v2 s1 s2)
  (cond ((or (null? s1) (null? s2)) '())
        ((element-of-set-v2? (car s1) s2)
         (cons (car s1)
               (intersection-set-v2 (cdr s1)
                                    s2)))
        (else
         (intersection-set-v2 (cdr s1) s2))))

(define (union-set-v2 s1 s2)
  (if (null? s1)
      s2
      (union-set-v2 (cdr s1) (cons (car s1) s2))))
