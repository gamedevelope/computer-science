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

(define (element-of-set? x s)
  (cond ((null? s) false)
        ((eq? x (car s)) true)
        (else
         (element-of-set? x (cdr s)))))

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

; 练习 2.70
(define pairs2.70 '((A 2)
                    (NA 16)
                    (BOOM 1)
                    (SHA 3)
                    (GET 2)
                    (YIP 9)
                    (JOB 2)
                    (WAH 1)))

(define tree2.70 (generate-huffman-tree pairs2.70))

(define (encode-branch bit word tree)
    (cond ((null? tree) '())
          ((leaf? tree)
           (if (eq? (symbol-leaf tree) word)
               (list bit)
               '()))
          (else
           (if (element-of-set? word (symbols tree))
               (append (list bit)
                (encode-branch 0 word (left-branch tree))
                (encode-branch 1 word (right-branch tree)))
               '()))))

(define (encode-2.70 message tree)
  (if (or (null? message) (null? tree))
      '()
      (append (encode-branch 0 (car message) (left-branch tree))
              (encode-branch 1 (car message) (right-branch tree))
              (encode-2.70 (cdr message) tree))))

(encode-2.70 '(NA) tree2.70)
(encode-2.70 message2.68 tree2.69)
(define message2.70 (append '(GET A JOB)
                            '(SHA NA NA NA NA NA NA NA NA)
                            '(GET A JOB)
                            '(SHA NA NA NA NA NA NA NA NA)
                            '(WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP)
                            '(SHA BOOM)))
(define bits (encode-2.70 message2.70 tree2.70))
(reverse (make-leaf-set pairs2.70))

; 这种 decode 方式解出来的结果与原消息不一致
(define (make-decode-tree leaf-set)
  (if (null? (cdr leaf-set))
      (car leaf-set)
      (make-code-tree (car leaf-set)
                      (make-decode-tree (cdr leaf-set)))))

(decode bits (make-decode-tree (reverse (make-leaf-set pairs2.70))))

(define result2.70 (decode bits tree2.70))
(equal? result2.70 message2.70)
