#lang sicp
; 表格的表示
(define (lookup table key)
  (let ((record (assoc key (cdr table))))
        (if record
            (cdr record)
            false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! table key value)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  table)

(define (extend! table key-1 key-2 value)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr table)))))
  table)


(define (locate! table key)
  (let ((subtable (assoc key table)))
    (if subtable
        table
        (begin (set-cdr! table
                         (list (list key)
                               (cdr table)))
               table))))

(define (make-table)
  (list '*table*))

; 创建支持任意维度的表格
; 表头用一个序对表示
(define (make-xtable)
  (list (cons '*table* '())))

(define t (make-table))
(insert! t 'a 100)