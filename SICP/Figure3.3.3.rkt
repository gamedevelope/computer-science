#lang sicp

; 表格的表示
;(define (lookup key table)
;  (let ((record (assoc key (cdr table))))
;        (if record
;            (cdr record)
;            false)))

;(define (assoc key records)
;  (cond ((null? records) false)
;        ((equal? key (caar records)) (car records))
;        (else (assoc key (cdr records)))))

;(define (insert! key value table)
;  (let ((record (assoc key (cdr table))))
;    (if record
;        (set-cdr! record value)
;        (set-cdr! table
;                  (cons (cons key value) (cdr table)))))
;  'ok)

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

(define (assoc key records)
  (cond ((null? records) false)
        ((same-key? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

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
;(define (make-table-v3.25)
;  (let ((local-table (list '*table*)))
;    (define (lookup key-1 key-2)
;      (let ((subtable
;             (assoc key-1 (cdr local-table))))
;        (if subtable
;            (let ((record
;                   (assoc key-2 (cdr subtable))))
;              (if record
;                  (cdr record)
;                  false))
;            false)))
;
;    (define (insert! key . value)
;      (let ((table local-table))
;        (define (loop)
;          (
;            
;      (let ((subtable (assoc key-1 (cdr local-table))))
;        (if subtable
;            (let ((record (assoc key-2 (cdr subtable))))
;              (if record
;                  (set-cdr! record value)
;                  (set-cdr! subtable
;                            (cons (cons key-2 value)
;                                  (cdr subtable)))))
;            (set-cdr! local-table
;                      (cons (list key-1
;                                  (cons key-2 value))
;                            (cdr local-table))))))
;    (define (dispatch m)
;      (cond ((eq? m 'lookup-proc) lookup)
;            ((eq? m 'insert-proc!) insert!)
;            ((eq? m 'print) (display local-table))
;            (else (error "Unknown operation -- TABLE" m))))
;    dispatch))

(define (f x . y)
  (display x)
  (if (> (length y) 1)
      (f (car y) (cdr y))))

;(define (f x . y)
;  (define (iter a b)
;    (if (null? a)
;        (display a)
;        (begin
;          (display a)
;          (iter (car b) (cdr b)))))
;  (iter x y))