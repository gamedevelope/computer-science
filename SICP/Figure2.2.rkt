#lang sicp

; 章节 2.2
(cons 1
      (cons 2
            (cons 3
                  (cons 4 nil))))
(list 1 2 3 4)
(cons 0
      (list 1 2 3 4))

(define (list-ref items n)
  (if (= 0 n)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define lst 0)
(set! lst (list 1 2 3 4 5))
(list-ref lst 0)
(list-ref lst 4)

(define (length lst)
  (if (null? lst)
      0
      (+ 1 (length (cdr lst)))))

(define (length-v2 lst)
  (define (iter lst n)
    (if (null? lst)
        n
        (iter (cdr lst) (inc n))))
  (iter lst 0))

(define (append lst1 lst2)
  (if (null? lst2)
      lst1
      (cons (car lst2)
            (append lst1 (cdr lst2)))))

; 练习 2.17
(define (last-pair lst)
  (if (null? lst)
      (error "param should not be an empty list")
      (if (= (length lst) 1)
          lst
          (last-pair (cdr lst)))))

; 练习 2.17 迭代版本
(define (last-pair-v2 lst)
  (if (null? (cdr lst))
      (cons (car lst) nil)
      (last-pair-v2 (cdr lst))))

; 练习 2.18
(define (reverse lst)
  (if (null? lst)
      lst
      (append (cons (car lst) nil)
                    (reverse (cdr lst)))))

; 练习 2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define no-more? null?)
(define except-first-denomination cdr)
(define first-denomination car)

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

; 练习 2.20
; 有点麻烦，对变参语法不熟悉的原因
(define (f x y . z)
  (display x)
  (display y)
  (display z))

(display "练习 2.20")
(newline)
(define (same-parity x . y)
  (define (iter a b)
    (if (null? b)
        (if (even? (- x a))
            (cons a nil)
            nil)
        (if (even? (- x a))
            (cons a (iter (car b) (cdr b)))
            (iter (car b) (cdr b)))))
  (iter x y))

(newline)
(same-parity 1 2 3 4 5)
(same-parity 2 2 3 4 5)

(define (d x . y)
  (display x)
  (display y))
