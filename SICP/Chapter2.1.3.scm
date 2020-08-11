#lang sicp

(#%require sicp-pict)

; 章节分割
(define (new-section msg)
  (newline)
  (newline)
  (display "* * * * * * *")
  (newline)
  (display msg)
  (newline)
  (display "* * * * * * *")
  (newline))

; 章节 2.1.3 数据意味着什么?
(new-section "2.1.3 数据意味着什么?")
(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)

(define (car z) (z 0))
(define (cdr z) (z 1))

(define pt213 (cons 1 2))
(car pt213)
(cdr pt213)
(car (cons 1 2))
