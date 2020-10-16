#lang sicp

; 表格的表示
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

; 练习 3.24
(define (same-key? key-1 key-2)
  (cond ((not (and (number? key-1) (number? key-2))) false)
        ((< (* key-1 key-2) 0) false)
        (else
         (if (= key-1 key-2)
             true
             (let ((d (- key-1 key-2)))
               (and (< d 0.000001)
                    (> d -0.000001)))))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))

    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table))))))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'print) (display local-table))
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(let ((t (make-table)))
  ((t 'insert-proc!) 'letters 'a 97)
  ((t 'insert-proc!) 'letters 'b 98)
  ((t 'insert-proc!) 'letters 0.0000000001 100)
  (t 'print)
  (display ((t 'lookup-proc) 'letters 0.0000000001))
  (display ((t 'lookup-proc) 'letters 'a)))

; 练习 3.25
; n 维表格
(define (make-table-3.25)
  (let ((local-table (list '*table*)))
    (define (lookup key)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (cdr record)
            false)))
    
    (define (insert! key value)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (set-cdr! record value)
            (set-cdr! local-table
                      (cons (cons key value) (cdr local-table))))))

    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            ((eq? m 'print) (display local-table))))
    dispatch))
(newline)
(let ((t (make-table-3.25)))
  ((t 'insert!) (list 'a 'b 'c) 100)
  ((t 'insert!) (list 'a 'b) 100)
  ((t 'insert!) (list 'a 'c) 100)
  ((t 'insert!) (list 'b 'c) 100)
  (display ((t 'lookup) (list 'b 'c)))
  (t 'print))

; 练习 3.26
; 用二叉树表示 n 维表格
(define (make-table-3.26)
  (define (make-tree node left right)
    (list node left right))
  (define (make-node keys value) (list (cons keys value) '() '()))
  (define (node-keys entry) (caar entry))
  (define (node-value entry) (cdar entry))
  (define (set-node-value! node value)
    (set-cdr! (car node) value))
  (define (set-node-left-branch-value! node new-node)
    (set-car! (cdr node) new-node))
  (define (set-node-right-branch-value! node new-node)
    (set-car! (cddr node) new-node))
  (define (entry tree) (car tree))
  (define (left-branch tree) (cadr tree))
  (define (right-branch tree) (caddr tree))
  (define (any->string a)
    (cond ((number? a) (number->string a))
          ((symbol? a) (symbol->string a))
          ((string? a) a)
          (else
           (error "Unknown TYPE -- PARAM" a))))
  
  ; 用字典序比较两个关键码列表
  (define (>? any-1 any-2)
    (string>? (any->string any-1)
              (any->string any-2)))

  (define (<? any-1 any-2)
    (string<? (any->string any-1)
              (any->string any-2)))
  
  (define (=? any-1 any-2)
    (string=? (any->string any-1)
              (any->string any-2)))
  
  (define (list>? list-1 list-2)
    (cond ((null? list-1) false)
          ((null? list-2) true)
          (else
           (let ((key-1 (any->string (car list-1)))
                 (key-2 (any->string (car list-2))))
             (cond ((string>? key-1 key-2) true)
                   (else
                    (list>? (cdr list-1) (cdr list-2))))))))
  (define (list=? list-1 list-2)
    (equal? list-1 list-2))

  (define (list<? list-1 list-2)
    (cond ((and (null? list-1) (null? list-2)) true)
          ((null? list-1) true)
          ((null? list-2) false)
          (else
           (let ((key-1 (any->string (car list-1)))
                 (key-2 (any->string (car list-2))))
             (cond ((string>? key-1 key-2) false)
                   (else
                    (list<? (cdr list-1) (cdr list-2))))))))
  
  (let ((table (list '*binary-table*)))
    (define (insert-node! node keys value)
      ; 如果是根节点，替换根节点的值
      (cond ((equal? (node-keys node) keys) (set-node-value! node value))
            ; 判断是否在左子树
            ((list<? keys (node-keys node))
             (if (null? (left-branch node))
                 ; 左子树为空，将新节点放入
                 (set-node-left-branch-value! node (make-node keys value))
                 ; 尝试向左子树插入
                 (insert-node! (left-branch node) keys value)))
            (else
             (if (null? (right-branch node))
                 (set-node-right-branch-value! node (make-node keys value))
                 (insert-node! (right-branch node) keys value)))))
    
    (define (insert! keys value)
      (let ((root-node (cdr table)))
        (cond ((null? root-node) (set-cdr! table (make-node keys value)))
              (else (insert-node! root-node keys value)))))

    (define (lookup-node node keys)
      (cond ((null? node) false)
            ((equal? keys (node-keys node)) (node-value node))
            ((list<? keys (node-keys node))
             (lookup-node (left-branch node) keys))
            (else
             (lookup-node (right-branch node) keys))))
    
    (define (lookup keys)
      (let ((root-node (cdr table)))
        (cond ((null? root-node) false)
              (else
               (lookup-node root-node keys)))))
    
    (define (dispatch m)
      (cond ((eq? m 'list>?) list>?)
            ((eq? m 'list<?) list<?)
            ((eq? m 'insert!) insert!)
            ((eq? m 'lookup) lookup)
            ((eq? m 'print) (display table))))
    
    dispatch))

(newline)

(let ((t (make-table-3.26)))
  (display ((t 'list>?) (list 1 2 3) (list 1 2 4)))
  (display ((t 'list>?) (list 1 2 3) (list 1 2 0)))
  (display ((t 'list>?) (list 1 2 3) (list 1 2)))
  (display ((t 'list>?) (list 1) (list 1 2))))

(newline)
(display "练习 3.26")
(let ((t (make-table-3.26)))
  (display ((t 'list<?) (list 'a) (list 'c)))
  ((t 'insert!) (list 'a) 1)
  ((t 'insert!) (list 'c) 2)
  ((t 'insert!) (list 'b) 3)
  ((t 'insert!) (list 'b 'c) 4)
  ((t 'insert!) (list 'b 'a) 5)
  (display ((t 'lookup) (list 'b 'a)))
  (display ((t 'lookup) (list 'b)))
  (display ((t 'lookup) (list 'b 'c)))
  (display ((t 'lookup) (list 'b 'd))))

; 练习 3.27
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else
         (+ (fib (- n 1))
            (fib (- n 2))))))

(define (make-table-3.27)
  (list '*table))

(define (memoize f)
  (let ((table (make-table-3.27)))
    (lambda (x)
      (let ((priviously-computed-result (lookup x table)))
        (or priviously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))