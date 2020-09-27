#lang sicp

(#%require "Common.rkt")

; 变动的表结构
; 练习 3.12
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

; 练习 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

; 练习 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(let ((x (list 'a 'b 'c 'd 'e)))
  (mystery x))

; 练习 3.16
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))
  
(let ()
  (echo (count-pairs (cons (cons (cons 1 2) 1) 1)))
  (echo (count-pairs (cons 1 (cons (cons 1 2) 1))))
  (echo (count-pairs (list (cons 1 (cons 2 (cons 1 2))))))
  (echo (count-pairs (list (cons 1 2) (cons 1 (cons 1 2)))))
  (echo (count-pairs (list (cons 1 2) (cons 1 2) (cons 1 2))))
  (echo (count-pairs (list (list (cons 1 2) (cons 1 2) (cons 1 2))))))

(define (in-set s elem)
  (cond ((null? s) false)
        ((eq? (car s) elem) true)
        (else (in-set (cdr s) elem))))

; 练习 3.17
;(define s3.17 '())
;(define n3.17 0)
;(define (count-pairs-v2 x s)
;  (cond ((not (pair? x)) 0)
;        ((in-set s x) 0)
;        (else
;         (map (lambda (n)
;                (if (pair? n)
;                    (let ()
;                      (set! s (append s (list n))))))
;              x)
;         (count-pairs-v2 (car x s))
;         (count-pairs-v2 (cdr x s))
;         (length s))))
;
;(let ()
;  (echo "练习3.17")
;  (echo (count-pairs-v2 (list 1 2 3 (cons 1 2)) '()))
;  (echo (count-pairs-v2 (cons (cons (cons 1 2) 1) 1)))
;  (echo (count-pairs-v2 (cons 1 (cons (cons 1 2) 1))))
;  (echo (count-pairs-v2 (list (cons 1 (cons 2 (cons 1 2))))))
;  (echo (count-pairs-v2 (list (cons 1 2) (cons 1 (cons 1 2)))))
;  (echo (count-pairs-v2 (list (cons 1 2) (cons 1 2))))
;  (echo (count-pairs-v2 (list (list (cons 1 2) (cons 1 2) (cons 1 2)))))
;  (echo "练习3.17"))

; 练习 3.18
(define (has-cycle? l s)
  (cond ((null? l) false)
        ((in-set s (car l)) true)
        (else
         (has-cycle? (cdr l) (append s (list (car l)))))))



; 练习 3.19
(define (has-cycle-v2? l)
  (define (compare l1 l2)
    (cond ((or (null? l1) (null? l2)) false)
          ((eq? l1 l2) true)
          (else
           (compare l1 (cdr l2)))))
  (if (null? l)
      false
      (compare l (cdr l))))

(let ()
  (echo (has-cycle-v2? '()))
  (echo (has-cycle-v2? '(1 2 3 4 5)))
  (echo (has-cycle-v2? (make-cycle (list 1 2 3)))))

; 改变也是赋值
;(define (cons x y)
;  (define (set-x! v) (set! x v))
;  (define (set-y! v) (set! y v))
;  
;  (define (dispatch m)
;    (cond ((eq? m 'car) x)
;          ((eq? m 'cdr) y)
;          ((eq? m 'set-car!) set-x!)
;          ((eq? m 'set-cdr!) set-y!)
;          (else (error "Undefined operation -- CONS" m))))
;  dispatch)

;(define (car z) (z 'car))
;(define (cdr z) (z 'cdr))