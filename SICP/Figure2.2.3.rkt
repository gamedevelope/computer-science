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

; 练习 2.35
; 第一个版本
(define (count-leaves2.35 t)
  (accumulate (lambda (x y)
                (cond ((null? x) 0)
                      (else (+ (if (not (pair? x))
                                   1
                                   (count-leaves2.35 x))
                               y))))
              0
              t))

; 第二个版本
(define (count-leaves2.35-v2 t)
  (accumulate (lambda (x y)
                (+ x y))
              0
              (map (lambda (x)
                     (cond ((null? x) 0)
                           (else (if (pair? x)
                                     (count-leaves2.35-v2 x)
                                     1))))
                   t)))

(define lst2.35 (list 1 (list 2 3 (list 3 4 5) (list 1))))
(count-leaves2.35 lst2.35)
(count-leaves2.35-v2 lst2.35)

; 练习 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define lst2.36 (list (list 1 2 3) (list 1 2 3) (list 1 2 3)))

; 练习 2.37
(define m (list (list 1 2 3 4) (list 4 5 6 6 ) (list 6 7 8 9)))
(define v (list 1 2 3 4))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(dot-product v v)

(define (matrix-*-vector m v)
  (map (lambda (v0)
              (dot-product v v0))
            m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x)
           (matrix-*-vector cols x))
         m)))

(define n2.37 (transpose m))
(matrix-*-matrix m n2.37)

; 练习 2.38
(define fold-right accumulate)
(define (fold-left op initial seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial seq))
(fold-right list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))

; 如果 (op x y)结果与(op y x)相等
; 那么 fold-left fold-right 结果一枝

(display "练习 2.39")
(newline)
(define lst2.39 (list 1 2 3 4 5 6))

; 练习 2.39
; 有点小麻烦

(define (reverse2.39 seq)
  (fold-right (lambda (x y)
                (append y (list x)))
              nil
              seq))

(reverse2.39 lst2.39)

(define (reverse2.39-v2 seq)
  (fold-left (lambda (x y)
               (cons y x))
             nil
             seq))
(reverse2.39-v2 lst2.39)

(define (prime? n)
  (define (iter i)
    (cond ((< n 2) false)
          ((= n 2) true)
          ((= n 3) true)
          ((even? n) false)
          ((> i (/ n 2)) true)
          (else (if (= 0 (remainder n i))
                    false
                    (iter (+ i 2))))))
  (iter 3))

; 嵌套映射
(enumerate-interval 1 10)
(accumulate append
            nil
            (map (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 5)))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

(define (remove item seq)
  (filter (lambda (x) (not (= x item)))
          seq))

(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))