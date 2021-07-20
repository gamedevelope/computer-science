#lang sicp
(#%require "Common.rkt")

;
;(define (apply-generic op . args)
;  (let ((type-tags (map type-tag args)))
;    (let ((proc (get op type-tags)))
;      (if proc
;          ;; apply 将 proc 应用到列表中
;          (apply proc (map contents args))
;          (error
;           "No method for these types -- APPLY-GENERIC"
;           (list op type-tags))))))

; 练习 2.82

; Figure 2.5.2
; 支持类型转换
;(define (apply-generic op . args)
;  (let ((type-tags (map type-tag args)))
;    (let ((proc (get op type-tags)))
;      (if proc
;          (apply proc (map contents args))
;          (if (= (length args) 2)
;              (let ((type1 (car type-tags))
;                    (type2 (cadr type-tags))
;                    (a1 (car args))
;                    (a2 (cadr args)))
;                (if (eq? type1 type2)
;                    (error "No method for these types" (list op type-tags))
;                    (let ((t1->t2 (get-coercion type1 type2))
;                          (t2->t1 (get-coercion type2 type1)))
;                      (cond (t1->t2
;                             (apply-generic op (t1->t2 a1) a2))
;                            (t2->t1
;                             (apply-generic op a1 (t2->t1 a2)))
;                            (else
;                             (error "No method for these types"
;                                    (list op type-tags)))))))
;              (error "No method for there types"
;                     (list op type-tags)))))))




(install-scheme-number-package)
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))


(install-complex-package)

(let ((c1 (make-complex-from-real-imag 1 2))
      (c2 (make-complex-from-real-imag 3 4)))
  (newline)
  (display (apply-generic 'add c1 c2))
  (newline)
  (display (apply-generic 'sub c1 c2))
  (newline)
  (display (apply-generic 'mul c1 c2))
  (newline)
  (display (apply-generic 'div c1 c2))
  (newline)
  (display (apply-generic 'magnitude c1))
  'done)

(apply-generic 'equ? 1 2)
(apply-generic 'equ? 1 1)
(apply-generic 'equ?
               (make-complex-from-real-imag 1 2)
               (make-complex-from-real-imag 1 3))
(apply-generic 'equ?
               (make-complex-from-real-imag 1 2)
               (make-complex-from-real-imag 1 2))

(if (apply-generic '=zero?
                   (make-complex-from-real-imag 0 0))
    (display "等于0")
    (display "不等于0"))

(display "type-raise")
(newline)
((get-coercion 'raise 'raise) 111 (make-complex-from-real-imag 1 2))
(newline)
((get-coercion 'raise 'raise) (make-complex-from-real-imag 1 2) 100)
(display "type-raise-end")
;(define (scheme-number->complex n)
;  (make-complex-from-real-imag (contents n) 0))

;(put-coercion 'scheme-number 'complex scheme-number->complex)
;((get-coercion 'scheme-number 'complex) 1)
(let ((c1 (make-complex-from-real-imag 1 1))
      (c2 (make-complex-from-real-imag 2 2)))
  (apply-generic 'add c1 c2))

(display "add complex rational")
(newline)
(let ((c1 (make-rational 1 1))
      (c2 1))
  (apply-generic 'add c1 c2))

(let ((c1 (make-rational 1 2))
      (c2 4))
  (apply-generic 'add c1 c2))

;; 练习 2.81
;(let ((c1 1)
;      (c2 2))
;  (display "练习 2.81")
;  (newline)
;  (apply-generic 'add c1 c2))
;
;; 假设先加入一个将自身类型转换为自身的方法
;(put-coercion 'scheme-number 'scheme-number (lambda (n) n))
;; 先制造一个例子,两个类型相同时,依然会调用“强制”的类型转换过程
;(let ((c1 (make-scheme-number 2))
;      (c2 (make-scheme-number 3)))
;  (display c1)
;  (display c2)
;  (apply-generic 'exp c1 c2))
;
;; 练习 2.83
;(let ((c1 (make-scheme-number 1.2))
;      (c2 (make-complex-from-real-imag 1 2)))
;  (apply-generic 'add c1 c2))
;
;; 练习2.84
;(define tower2.84 (list (list (cons 'a 'b) (lambda () (display "a->b") false))
;                        (list (cons 'b 'c) (lambda () (display "b->c") false))
;                        (list (cons 'c 'd) (lambda () (display "c->d") false))
;                        (list (cons 'd 'e) (lambda () (display "d->e") false))))
;
;; 提升类型到同一层
;(let ((c1 (make-scheme-number 10))
;      (c2 (make-complex-from-real-imag 1 2)))
;  (apply-generic 'add c1 c2))