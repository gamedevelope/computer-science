#lang sicp

; sets as binary tree

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x s)
  (cond ((null? s) false)
        ((= x (entry s)) true)
        ((< x (entry s))
         (element-of-set? x (left-branch s)))
        ((> x (entry s))
         (element-of-set? x (right-branch s)))))

(define t1 (make-tree 7
                      (make-tree 3
                                 (make-tree 1 '() '())
                                 (make-tree 5 '() '()))
                      (make-tree 9
                                 '()
                                 (make-tree 11 '() '()))))
(element-of-set? 1 t1)
(element-of-set? 2 t1)
(element-of-set? 3 t1)
(element-of-set? 4 t1)
(element-of-set? 5 t1)
(element-of-set? 6 t1)

(define (adjoin-set x s)
  (cond ((null? s) (make-tree x '() '()))
        ((= x (entry s)) s)
        ((< x (entry s))
         (make-tree (entry s)
                    (adjoin-set x (left-branch s))
                    (right-branch s)))
        ((> x (entry s))
         (make-tree (entry s)
                    (left-branch s)
                    (adjoin-set x (right-branch s))))))

(define t2 '())

(set! t2 (adjoin-set 1 t2))
(set! t2 (adjoin-set 2 t2))
(set! t2 (adjoin-set 3 t2))
(set! t2 (adjoin-set 4 t2))
(set! t2 (adjoin-set 5 t2))
(set! t2 (adjoin-set 6 t2))
(set! t2 (adjoin-set 7 t2))
;(set! t2 (adjoin-set 1 t2))
;(set! t2 (adjoin-set 5 t2))
;(set! t2 (adjoin-set 11 t2))
;(set! t2 (adjoin-set 12 t2))
;(set! t2 (adjoin-set 8 t2))

(define (tree->list-v1 tree)
  (display 'tree->list-v1)
  (newline)
  (if (null? tree)
      '()
      (append (tree->list-v1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-v1 (right-branch tree))))))

(define (tree->list-v2 tree)
  (define (copy-to-list tree result-list)
    (display 'copy-to-list)
    (newline)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))
(tree->list-v1 t2)
(tree->list-v2 t2)

; 练习 2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      ; quotient 求商
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))
(define tree1 (list->tree '(1 2 3 4)))
(define tree2 (list->tree '(3 4 5 6)))

; 练习 2.65
(define (intersection-list l1 l2)
    (cond ((or (null? l1) (null? l2)) '())
          (else
           (let ((x1 (car l1))
                 (x2 (car l2)))
             (cond ((= x1 x2)
                    (cons x1 (intersection-list (cdr l1) (cdr l2))))
                   ((< x1 x2)
                    (intersection-list (cdr l1) l2))
                   (else
                    (intersection-list l1 (cdr l2))))))))

(define (union-list l1 l2)
  (cond ((null? l1) l2)
        ((null? l2) l1)
        (else
         (let ((x1 (car l1))
               (x2 (car l2)))
           (cond ((= x1 x2)
                  (cons x1 (union-list (cdr l1) (cdr l2))))
                 ((< x1 x2)
                  (cons x1 (union-list (cdr l1) l2)))
                 ((< x2 x1)
                  (cons x2 (union-list l1 (cdr l2)))))))))

(define (intersection-set t1 t2)
  (let ((s1 (tree->list-v1 t1))
        (s2 (tree->list-v1 t2)))
    (list->tree (intersection-list s1 s2))))

(define (union-set t1 t2)
  (let ((s1 (tree->list-v1 t1))
        (s2 (tree->list-v1 t2)))
    (let ((ut (union-list s1 s2)))
      (list->tree (union-list s1 s2)))))

(display (intersection-set tree1 tree2))
(display (union-set tree1 tree2))

(define (key record)
  (car record))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

(lookup 'a '((a 1) (b 2) (c 3)))
(lookup 'd '((a 1) (b 2) (c 3)))

(define (lookup-tree given-key tree)
  (cond ((null? tree) false)
        (else
         (let ((root (entry tree))
               (left-leaf (left-branch tree))
               (right-leaf (right-branch tree)))
           (cond ((= given-key (key root)) root)
                 ((< given-key (key root))
                  (lookup-tree given-key left-leaf))
                 ((> given-key (key root))
                  (lookup-tree given-key right-leaf)))))))

(define tree2.66 (make-tree '(1 a)
                            (make-tree '(0 (1 2 3)) '() '())
                            (make-tree '(2 22222) '() '())))

(lookup-tree 2 tree2.66)
(lookup-tree 0 tree2.66)