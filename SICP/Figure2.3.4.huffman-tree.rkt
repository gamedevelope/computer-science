#lang sicp

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x s)
  (cond ((null? s) (list x))
        ((< (weight x) (weight (car s))) (cons x s))
        (else (cons (car s)
                    (adjoin-set x (cdr s))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

; 练习 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)

; 练习 2.68
; 有点复杂
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((null? tree) (error "bit symbol -- SEARCH SYMBOL" symbol))
        ((eq? symbol (caar tree)) (cadar tree))
        (else
         (encode-symbol symbol (cdr tree)))))

(define message2.68 '(A D A B B C A))
(define tree2.68 (list '(A (0)) '(B (1 0)) '(C (1 1 1)) '(D (1 1 0))))
(define bits2.68 (encode message2.68 tree2.68))
(equal? sample-message bits2.68)

; 练习 2.69
(make-leaf-set '((A 4) (B 2) (C 1) (D 1)))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (cond ((null? (cdr leaf-set)) (car leaf-set))
        (else
         (let ((leaf1 (car leaf-set))
               (leaf2 (cadr leaf-set)))
           (let ((merge-tree (make-code-tree leaf1 leaf2)))
             (successive-merge (adjoin-set merge-tree (cddr leaf-set))))))))

(define tree2.69 (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1))))