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

; Figure 2.3.2 Symbolic Differentiation
(define (=number? exp num) (and (number? exp) (= exp num)))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else
         (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
(define (make-exponentiation m1 m2)
  (cond ((=number? m2 0) 1)
        ((=number? m2 1) m1)
        (else (list '** m1 m2))))
(define (sum? x) (and (pair? x) (eq? (car x) '+)))

; 练习 2.57
; 扩充求导程序，使之能处理任意项
(define (addend s) (cadr s))
;(define (augend s) (caddr s))
(define (augend s)
  (let ((l (length s)))
    (cond ((< l 3) (error "arguments error"))
          ((= l 3) (caddr s))
          (else (cons (car s) (cddr s))))))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
;(define (multiplicand p) (caddr p))
(define (multiplicand s)
  (let ((l (length s)))
    (cond ((< l 3) (error "arguments error"))
          ((= l 3) (caddr s))
          (else (cons (car s) (cddr s))))))
(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))

; 求导程序主要步骤
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ; 练习 2.56 增加指数函数的求导规则
        ((exponentiation? exp)
         (make-product (make-product (caddr exp)
                                     (make-exponentiation (cadr exp) (dec (caddr exp))))
                       (deriv (cadr exp) var)))
          
        (else
         (error "unknow expression type: DERIV" exp))))

(define (deriv-n exp var n)
  (if (< n 1)
      exp
      (deriv-n (deriv exp var) var (dec n))))

(deriv '(+ x x) 'x)
(deriv '(* x x x) 'x)
(deriv '(* x y z) 'x)
(deriv (deriv (deriv (deriv '(* (* x x) (* x x)) 'x) 'x) 'x) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '(** x 10) 'x)

; 练习 2.58
; 求导中缀表达式
(define (make-sum-v2 a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else
         (cons a1 (list '+ a2)))))

(define (make-product-v2 m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (cons m1 (list '* m2)))))
(define (make-exponentiation-v2 m1 m2)
  (cond ((=number? m2 0) 1)
        ((=number? m2 1) m1)
        (else (list '** m1 m2))))
(define (sum-v2? x) (and (pair? x) (eq? (cadr x) '+)))

(define (addend-v2 s) (car s))
(define (augend-v2 s) (caddr s))

(define (product-v2? x) (and (pair? x) (eq? (cadr x) '*)))
(define (multiplier-v2 p) (car p))
(define (multiplicand-v2 p) (caddr p))

(define (exponentiation-v2? x) (and (pair? x) (eq? (car x) '**)))

; 求导程序主要步骤
(define (deriv-v2 exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum-v2? exp) (make-sum-v2 (deriv-v2 (addend-v2 exp) var)
                              (deriv-v2 (augend-v2 exp) var)))
        ((product-v2? exp)
         (make-sum-v2
          (make-product-v2 (multiplier-v2 exp)
                        (deriv-v2 (multiplicand-v2 exp) var))
          (make-product-v2 (deriv-v2 (multiplier-v2 exp) var)
                        (multiplicand-v2 exp))))
        ; 练习 2.56 增加指数函数的求导规则
        ((exponentiation-v2? exp)
         (make-product-v2 (make-product-v2 (caddr exp)
                                     (make-exponentiation-v2 (cadr exp) (dec (caddr exp))))
                       (deriv-v2 (cadr exp) var)))
          
        (else
         (error "unknow expression type: DERIV" exp))))
