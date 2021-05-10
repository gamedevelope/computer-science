#lang sicp

(#%require "Figure4.3.3.rkt")

(define (analyze input)
  (define (succeed v next)
    'ok)

  (define (fail v next)
    false)
  
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
;;; 需要一些数学技巧
;;; 参考 3.69
(analyze '(define (number-stream n)
            (amb n (number-stream (+ n 1)))))

(analyze '(define (ex4.36-v1)
            (let ((i (number-stream 1)))
              (let ((j (an-integer-between 1 i)))
                (let ((k (sqrt (+ (* i i) (* j j)))))
                  (require (integer? k))
                  (list j i k))))))

;;; 练习 4.37
;;; 效率比 4.35高
(analyze '(define (a-pythagorean-triple-between low high)
            (let ((i (an-integer-between low high))
                  (hsq (* high high)))
              (let ((j (an-integer-between i high)))
                (let ((ksq (+ (* i i) (* j j))))
                  (require (>= hsq ksq))
                  (let ((k (sqrt ksq)))
                    (require (integer? k))
                    (list i j k)))))))

;;; 谜题
(analyze '(define (distinct? items)
            (cond ((null? items) true)
                  ((null? (cdr items)) true)
                  ((member (car items) (cdr items)) false)
                  (else (distinct? (cdr items))))))

(analyze '(define (multiple-dwelling)
            (let ((baker (amb 1 2 3 4 5))
                  (cooper (amb 1 2 3 4 5))
                  (fletcher (amb 1 2 3 4 5))
                  (miller (amb 1 2 3 4 5))
                  (smith (amb 1 2 3 4 5)))
              (require (distinct? (list baker cooper fletcher miller smith)))
              (require (not (= baker 5)))
              (require (not (= cooper 1)))
              (require (not (= fletcher 5)))
              (require (not (= fletcher 1)))
              (require (> miller cooper))
              ;              (require (not (= (abs (- smith fletcher)) 1)))
              (require (not (= (abs (- fletcher cooper)) 1)))
              (list (list 'baker baker)
                    (list 'cooper cooper)
                    (list 'fletcher fletcher)
                    (list 'miller miller)
                    (list 'smith smith)))))
;;; 练习 4.38
;;; 有5种答案

;;; 练习 4.39
;;; 会影响
(analyze '(define (multiple-dwelling-v2)
            (let ((baker (amb 1 2 3 4 5))
                  (cooper (amb 1 2 3 4 5))
                  (fletcher (amb 1 2 3 4 5))
                  (miller (amb 1 2 3 4 5))
                  (smith (amb 1 2 3 4 5)))
              (require (not (= cooper 1)))
              (require (not (= fletcher 1)))
              (require (not (= fletcher 5)))
              (require (not (= baker 5)))
              (require (> miller cooper))
              (require (not (= (abs (- smith fletcher)) 1)))
              (require (not (= (abs (- fletcher cooper)) 1)))
              (require (distinct? (list baker cooper fletcher miller smith)))
              (list (list 'baker baker)
                    (list 'cooper cooper)
                    (list 'fletcher fletcher)
                    (list 'miller miller)
                    (list 'smith smith)))))

;;; 练习 4.40
(analyze '(define (multiple-dwelling-v3)
            (let ((cooper (amb 1 2 3 4 5)))
              (require (not (= cooper 1)))
              (let ((baker (amb 1 2 3 4 5)))
                (require (not (= baker 5)))
                (let ((fletcher (amb 1 2 3 4 5)))
                  (require (not (= fletcher 1)))
                  (require (not (= fletcher 5)))
                  (let ((miller (amb 1 2 3 4 5)))
                    (require (> miller cooper))
                    (let ((smith (amb 1 2 3 4 5)))
                      (require (> miller cooper))
                      (require (not (= (abs (- smith fletcher)) 1)))
                      (require (not (= (abs (- fletcher cooper)) 1)))
                      (require (distinct? (list baker cooper fletcher miller smith)))
                      (list (list 'baker baker)
                            (list 'cooper cooper)
                            (list 'fletcher fletcher)
                            (list 'miller miller)
                            (list 'smith smith)))))))))

;;; 练习 4.41
;;; 常规方法解决 4.40问题


;;; 练习 4.42
(analyze '(define (ex4.42)
            (let ((kitty (amb 2))
                  (betty (amb 1 3))
                  (ethel (amb 1 5))
                  (joan (amb 2 3))
                  (mary (amb 4)))
              (require (distinct? (list kitty betty ethel joan mary)))
              (let ((a1 (= kitty 2))
                    (a2 (= betty 3))
                    (b1 (= ethel 1))
                    (b2 (= joan 2))
                    (c1 (= joan 3))
                    (c2 (= ethel 5))
                    (d1 (= kitty 2))
                    (d2 (= mary 4))
                    (e1 (= mary 4))
                    (e2 (= betty 1)))
                ;                (list kitty betty ethel joan mary)
;                (require (and (or a1 a2)
;                              (or b1 b2)
;                              (or c1 c2)
;                              (or d1 d2)
;                              (or e1 e2)))
;                (require (not (or (and a1 a2)
;                                  (and b1 b2)
;                                  (and c1 c2)
;                                  (and d1 d2)
;                                  (and e1 e2))))
                (list (list 'kitty kitty)
                      (list 'betty betty)
                      (list 'ethel ethel)
                      (list 'joan joan)
                      (list 'mary mary))))))
(driver-loop)
