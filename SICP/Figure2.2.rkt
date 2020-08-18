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

(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (factor (car items))
            (scale-list (cdr items) factor))))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(map inc (list 1 2 3 4 5))
(map abs (list -1 2 -3 4))

(define (square x)
  (* x x))

; 练习 2.21
(define (square-list lst)
  (map (lambda (x) (* x x)) lst))

(define (square-list-v2 lst)
  (if (null? lst)
      nil
      (cons (* (car lst) (car lst))
            (square-list (cdr lst)))))

; 练习 2.22
; 错误版本 1
; 错误的原因是 cons 把后面的数放到了前面
(define (square-list-ex2.22 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

(square-list-ex2.22 (list 1 2 3 4))

; 错误版本2
; cons 参数类型不正确
(define (square-list-ex2.22-v2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

(square-list-ex2.22-v2 (list 1 2 3 4))

; 正确的版本
; 可以加一个 reverse
(define (square-list-ex2.22-v3 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (reverse (iter items nil)))
(square-list-ex2.22-v3 (list 1 2 3 4 5))

; 练习 2.23
;(define (no p)
;  (p))
(define (for-each proc items)
  (cond ((null? items) true)
        (else
         (proc (car items))
         (for-each proc (cdr items)))))

(for-each (lambda (x) (display x))
          (list 1 2 3 4 5))

; figure 2.2.2
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

; 练习 2.24
(count-leaves (list 1 (list 2 (list 3 4))))

; 练习 2.25
(car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9))))))
(car (car (list (list 7))))
(car(cdr
     (car (cdr
           (car (cdr
                 (car (cdr
                       (car (cdr
                             (car (cdr
                                   (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))))))))))))

; 练习 2.26
(define x2.26 (list 1 2 3))
(define y2.26 (list 4 5 6))
(append x2.26 y2.26)
(cons x2.26 y2.26)
(list x2.26 y2.26)

; 练习 2.27
(define (reverse-v2 lst)
  (define (iter a b)
    (if (null? b)
        a
        (iter (cons (car b) a) (cdr b))))
  (iter nil lst))

; 深度反转
(define (deep-reverse lst)
  (define (iter a b)
    (if (null? b)
        a
        (let ((fb (car b)))
          (iter (cons (if (pair? fb)
                          (iter nil fb)
                          fb)
                      a)
                (cdr b)))))
  (iter nil lst))

; 练习 2.28
(define (fringe tree)
  (define (iter lst tree)
    (cond ((null? tree) nil)
          ((not (pair? tree)) (cons tree lst))
          (else
           (append (iter lst (car tree))
                   (iter lst (cdr tree))))))
  (reverse (iter nil tree)))

(fringe (list 1 2 (list 3 4) (list 4 5 6 (list 7 8))))

(define (frigne-v2 tree)
  (define (iter lst tree)
    (cond ((null? tree) nil)
          ((not (pair? tree)) (cons tree lst))
          (else
           (append (iter lst (car tree))
                   (iter lst (cdr tree))))))
  (reverse (iter nil tree)))

(frigne-v2 (list 1 2 (list 3 4) (list 4 5 6 (list 7 8))))

; 练习 2.29
(define (make-mobile left right)
;  (list left right))
  (cons left right))

(define (make-branch length structure)
;  (list length structure))
  (cons length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
;  (cadr mobile))
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
;  (cadr branch))
  (cdr branch))

(define (total-weight mobile)
  (cond ((not (pair? (left-branch mobile)))
         (if (not (pair? (right-branch mobile)))
             (branch-structure mobile)
             (total-weight (right-branch mobile))))
        (else (+ (total-weight (left-branch mobile))
                 (total-weight (right-branch mobile))))))

(define mb2.29 (make-mobile
                (make-branch 1 10)
                (make-branch 10
                             (make-mobile
                              (make-branch 1 2)
                              (make-branch 1 3)))))

(total-weight mb2.29)

(define mb2.29-v1 (make-mobile
                   (make-branch
                    10
                    (make-mobile
                     (make-branch 10 2)
                     (make-branch 10 2)))
                   (make-branch 4 10)))

(define (mobile-blance? mobile)
  (if (pair? (left-branch mobile))
      (and (= (* (branch-length (left-branch mobile)) (total-weight (left-branch mobile)))
              (* (branch-length (right-branch mobile)) (total-weight (right-branch mobile))))
           (mobile-blance? (left-branch mobile))
           (mobile-blance? (right-branch mobile)))
      true))
(mobile-blance? mb2.29-v1)

(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(scale-tree (list 1 2 3 4 5)
            10)

(define (scale-tree-map tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree-map sub-tree factor)
             (* sub-tree factor)))
       tree))

(scale-tree-map (list 1 2 3 (list 4 5 6)) 10)

; 练习 2.30
(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree-map tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-map sub-tree)
             (square sub-tree)))
       tree))

(define tree2.30 (list 1 2
                       (list 4 5
                             (list 6 7))))
(square-tree-map tree2.30)