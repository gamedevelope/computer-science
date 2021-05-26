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
;;; 先写一个全排列算法
;(define (cnr n lst)
(define (append3 front middle back)
  (append front (append middle back)))

(define (nth n lst)
  (if (= n 0)
      (car lst)
      (nth (- n 1) (cdr lst))))

(define (remove-nth n lst)
  (if (= n 0)
      (cdr lst)
      (cons (car lst) (remove-nth (- n 1) (cdr lst)))))

(define (headn n lst)
  (cond ((<= n 0) lst)
        (else (cons (nth n lst) (remove-nth n lst)))))

(define (make-pair a b)
  (if (list? b)
      (cons a b)
      (list a b)))

;;; 全排列算法
(define (permutation lst)
  (define (fgp lst)
    (define (map-prefix-append elem lst)
      (if (pair? lst)
          (cons (make-pair elem (car lst))
                (map-prefix-append elem (cdr lst)))
          '()))
    (if (pair? lst)
        (append (map-prefix-append (caar lst) (permutation (cdar lst)))
                (fgp (cdr lst)))
        '()))
  (define (f lst)
    (define (iter n lst)
      (if (< n 0)
          '()
          (cons (headn n lst) (iter (- n 1) lst))))
    (iter (- (length lst) 1) lst))
  
  (if (null? (cdr lst))
      lst
      (fgp (f lst))))

(permutation '(1))
(permutation '(1 2))
(permutation '(1 2 3))

;;; 先生成全排列
;;; 依次将测试每种排列
(define (ex4.41)
  (define (distinct? items)
    (cond ((null? items) true)
          ((null? (cdr items)) true)
          ((member (car items) (cdr items)) false)
          (else (distinct? (cdr items)))))
  
  (define (f baker cooper fletcher miller smith)
    (if (and (not (= cooper 1))
             (not (= fletcher 1))
             (not (= fletcher 5))
             (not (= baker 5))
             (> miller cooper)
             (not (= (abs (- smith fletcher)) 1))
             (not (= (abs (- fletcher cooper)) 1))
             (distinct? (list baker cooper fletcher miller smith)))
        (list (list 'baker baker)
              (list 'cooper cooper)
              (list 'fletcher fletcher)
              (list 'miller miller)
              (list 'smith smith))
        '()))
  (define (g lst)
    (if (null? lst)
        '()
        (append (let ((elem (car lst)))
                (let ((kitty (nth 0 elem))
                      (betty (nth 1 elem))
                      (ethel (nth 2 elem))
                      (joan (nth 3 elem))
                      (mary (nth 4 elem)))
                  (f kitty betty ethel joan mary)))
              (g (cdr lst)))))
  
  (g (permutation '(1 2 3 4 5))))

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

;;; 练习 4.43
;(analyze '(define (ex4.43)

(analyze '(define (can-attack? p1 p2)
            (let ((px1 (car p1))
                  (py1 (cdr p1))
                  (px2 (car p2))
                  (py2 (cdr p2)))
              (or (= px1 px2)
                  (= py1 py2)
                  (= (abs (- px1 px2))
                     (abs (- py1 py2)))))))

;;; 八皇后耗时太长
;;; 先解决六皇后问题
(analyze '(define (ex4.44)
            (let ((a (cons (amb 1 2 3 4 5 6 7 8) (amb 1 2 3 4 5 6 7 8))))
              (let ((b (cons (amb 1 2 3 4 5 6 7 8) (amb 1 2 3 4 5 6 7 8))))
                (require (not (can-attack? a b)))
                (let ((c (cons (amb 1 2 3 4 5 6 7 8) (amb 1 2 3 4 5 6 7 8))))
                  (require (not (can-attack? a c)))
                  (require (not (can-attack? b c)))
                  (let ((d (cons (amb 1 2 3 4 5 6 7 8) (amb 1 2 3 4 5 6 7 8))))
                    (require (not (can-attack? a d)))
                    (require (not (can-attack? b d)))
                    (require (not (can-attack? c d)))
                    (let ((e (cons (amb 1 2 3 4 5 6 7 8) (amb 1 2 3 4 5 6 7 8))))
                      (require (not (can-attack? a e)))
                      (require (not (can-attack? b e)))
                      (require (not (can-attack? c e)))
                      (require (not (can-attack? d e)))
                      (let ((f (cons (amb 1 2 3 4 5 6 7 8) (amb 1 2 3 4 5 6 7 8))))
                        (require (not (can-attack? a f)))
                        (require (not (can-attack? b f)))
                        (require (not (can-attack? c f)))
                        (require (not (can-attack? d f)))
                        (require (not (can-attack? e f)))
                        (list a b c d e f)))))))))

;;; 4.51
(analyze '(define count 0))

;;; 4.54
;(define (analyze-require exp)
;  (let ((pproc (analyze (require-predicate exp))))
;    (lambda (env succeed fail)
;      (proc env
;            (lambda (pred-value fail2)
;              (if ??
;                  ??
;                  (succeed 'ok fail2)))
;            fail))))
              
(driver-loop)
