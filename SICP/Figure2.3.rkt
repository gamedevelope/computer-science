#lang sicp

; Figure 2.3 符号数据
; 练习 2.53
(list 'a 'b 'c)
(list (list 'george))
(cdr '((x1 x2) (y1 y2)))

(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))
(memq 'red '((red shoes) (blue socks)))
(memq 'red '(red shoes blue socks))
(memq 'hello '(a b hello world))

; 练习 2.54
(eq? 'a 'b)
(eq? 'a 'a)

;;; 分别判断
;;; * l1 l2 都不是列表的情况
;;; * l1 l2 类型不同的情况
;;; * l1 l2 一个为空一个不为空的情况
;;; * l1 l2 元素是否相等
(define (equal? l1 l2)
  (cond ((and (not (pair? l1))
              (not (pair? l2)))
         (eq? l1 l2))
        ((not (eq? (pair? l1) (pair? l2))) false)
        ((not (eq? (null? l1) (null? l2))) false)
        (else (and (eq? (car l1) (car l2))
                   (equal? (cdr l1) (cdr l2))))))

; 练习 2.55
; 'abracadabra 整个是一个元素
(car ''abracadabra)
(cdr ''abracadabra)
(cdr (cdr ''abracadabra))
(identity 123)