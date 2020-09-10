#lang sicp

(define (square x)
  (* x x))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

; 数据导向的程序设计和可加性
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))

  (define (magnitude z)
    (sqrt (+ (square (real-part z)) (square (imag-part z)))))

  (define (angle z)
    (atan (imag-part z) (real-part z)))

  (define (make-from-real-imag x y) (cons x y))

  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (real-part z)
  (* (magnitude z) (cos (angle z))))
(define (imag-part z)
  (* (magnitude z) (sin (angle z))))
(define (magnitude z) (car z))
(define (angle z) (cdr z))

(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))

(define (make-from-mag-ang r a)
  (cons r a))
  
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          ;; apply 将 proc 应用到列表中
          (apply proc (map contents args))
;          (proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
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
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(install-rectangular-package)
(install-polar-package)

(define z1 (make-from-real-imag 1 1))
(define z2 (make-from-mag-ang 1.4142 (/ 3.1415926 4)))

; 练习 2.73
(define (variable? exp)
  (symbol? exp))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;(define (deriv exp var)
;  (cond ((number? exp) 0)
;        ((variable? exp) (if (same-variable? exp var) 1 0))
;        ((sum? exp)
;         (make-sum (deriv (addend exp) var)
;                   (deriv (augend exp) var)))
;        ((product? exp)
;         (make-sum
;          (make-product (multiplier exp)
;                        (deriv (multiplicand exp) var))
;          (make-product (deriv (multiplier exp) var)
;                        (multiplicand exp))))
;        (else (error "unknow expression type -- DERIV" exp))))
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (=number? a n)
  (and (number? a) (= a n)))

; 用数据导向的方式重写该过程
(define (install-deriv-sum-package)
  (define (addend exp)
    (car exp))
  (define (augend exp)
    (cadr exp))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (sum-deriv exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  (put 'deriv 'make-sum make-sum)
  (put 'deriv '+ sum-deriv)
  'done)
(install-deriv-sum-package)

(deriv '(+ x y) 'x)

(define (install-deriv-product-package)
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  (define (product-deriv exp var)
    ((get 'deriv 'make-sum)
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
                   (multiplicand exp))))
  (put 'deriv 'make-product make-product)
  (put 'deriv '* product-deriv)
  'done)
(install-deriv-product-package)
(install-deriv-product-package)

(deriv '(* x x) 'x)
(deriv '(+ (* a (* x x)) (* b x) c) 'x)

(define (install-deriv-power-package)
  (define (base exp) (car exp))
  (define (exponential exp) (cadr exp))
  (define (reduction exp)
    (if (number? exp)
        (- exp 1)
        (list '- exp 1)))
  (define (make-power m1 m2)
    (cond ((=number? m2 0) 1)
          ((=number? m2 1) m1)
          (else
           (list '** m1 m2))))
  (display (make-power 'x 'y))
  (define (power-deriv exp var)
    ((get 'deriv 'make-product)
     (exponential exp)
     (make-power (base exp)
                 (reduction (exponential exp)))))
  (put 'deriv 'make-power make-power)
  (put 'deriv '** power-deriv)
  'done)
(install-deriv-power-package)
(deriv '(** x n) 'x)
(deriv '(** x 3) 'x)
(deriv '(* (** x m) (** x n)) 'x)

; 练习 2.74
(define (get-record employ data)
  ((get 'get-record (company-name data)) employ (company-data data)))

(define (get-salary employ data)
  (let ((company (company-name data)))
    ((get 'get-salary company)
     (get-record employ data))))

(define (company-name data)
  (car data))

(define (company-data data)
  (cdr data))

(define (install-microsoft-package)
  (define (get-name data)
    (car data))
  (define (get-salary data)
    (caddr data))
  (define (get-record employ data)
    (cond ((null? data) '())
          ((eq? (get-name (car data)) employ) (car data))
          (else
           (get-record employ (cdr data)))))
  (put 'get-record 'microsoft get-record)
  (put 'get-salary 'microsoft get-salary)
  'microsoft-done)

(install-microsoft-package)
(define microsoft-data
  (list 'microsoft
        '(黄晓明 北京微软 一万)
        '(a 1 2)
        '(b 3 4)
        '(c 5 6)))
(get-record 'a microsoft-data)
(get-record 'b microsoft-data)

(define (install-huawei-package)
  (define (get-name data)
    (car data))
  (define (get-salary data)
    (caddr data))
  (define (get-record employ data)
    (cond ((null? data) '())
          ((eq? (get-name (car data)) employ) (car data))
          (else
           (get-record employ (cdr data)))))
  (put 'get-record 'huawei get-record)
  (put 'get-salary 'huawei get-salary)
  'huawei-done)
(install-huawei-package)
(define huawei-data
  (list 'huawei
        '(黄晓明 北京华为 一万)
        '(刘腾达 上海 两万)))

(get-salary '黄晓明 huawei-data)

; 练习 2.74 (c)
(define (find-employee-record employ data)
  (if (null? data) '()
      (cons (get-record employ (car data))
            (find-employee-record employ (cdr data)))))
(get-record '黄晓明 microsoft-data)
(get-record '黄晓明 huawei-data)
(find-employee-record '黄晓明 (list microsoft-data huawei-data))