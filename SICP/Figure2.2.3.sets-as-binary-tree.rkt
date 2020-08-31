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
