#lang sicp
(define (square x) (* x x))

; Figure 2.2.3
(define (filter predicate seq)
  (cond ((null? seq) nil)
        ((predicate (car seq))
         (cons (car seq)
               (filter predicate (cdr seq))))
        (else (filter predicate (cdr seq)))))

(define lst1 (list 1 2 3 4 5 6 7 8))

(filter (lambda (x)
          (= 0 (remainder x 3)))
        lst1)

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))

(accumulate + 1 lst1)
(accumulate + 0 lst1)
(accumulate * 1 lst1)
(accumulate cons 1 lst1)

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 1 10)

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define tree2.2.2 (list 1 (list 2 (list 3 (list 4 5)))))

(enumerate-tree tree2.2.2)

(define (sum-odd-squences tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(sum-odd-squences tree2.2.2)

(define (fib n)
  (define (iter a b m)
    (if (> m n)
        b
        (iter b (+ a b) (inc m))))
  (iter 0 1 0))

(define (even-fibs n)
  (accumulate cons
              nil
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))
;(even-fibs 100)

(newline)
(display "组合运算")
(filter (lambda (x)
          (= 0 (remainder x 2)))
        (map inc
             (map inc
                  (map square
                       (map (lambda (x)
                              (* x x x))
                            (enumerate-interval 0 10))))))

; 练习 2.33
(define (map2.33 p seq)
  (accumulate (lambda (x y)
                (cons (p x) y))
              nil
              seq))

(define (append2.33 seq1 seq2)
  (accumulate cons
              seq2
              seq1))

(define (length2.33 seq)
  (accumulate (lambda (x y)
                (inc y))
              0
              seq))

; 练习 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff
                   ; higher-terms 表示后续序列的值
                   (* x higher-terms)))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 2 3 4 5 6 7))