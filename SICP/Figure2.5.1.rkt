#lang sicp
(define (square x) (* x x))
(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else
         (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else
         (error "Bad tagged datum -- CONTENTS" datum))))
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

(define (put-coercion t1 t2 f)
  (put 'coercion (cons t1 t2) f))
(define (get-coercion t1 t2)
  (get 'coercion (cons t1 t2)))
;
;(define (apply-generic op . args)
;  (let ((type-tags (map type-tag args)))
;    (let ((proc (get op type-tags)))
;      (if proc
;          ;; apply 将 proc 应用到列表中
;          (apply proc (map contents args))
;          (error
;           "No method for these types -- APPLY-GENERIC"
;           (list op type-tags))))))

; 练习 2.82
(define (install-type-raise-package)
  (put-coercion 'scheme-number
                'rational
                (lambda (n)
                  (make-rational n 1)))
  (put-coercion 'rational
                'real
                (lambda (r)
                  (/ ((get 'numer) r) ((get 'demon) r))))
  (put-coercion 'real
                'complex
                (lambda (r)
                  (make-complex-from-real-imag r 0)))
  'done)

(install-type-raise-package)

; Figure 2.5.2
; 支持类型转换
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)
                    (error "No method for these types" (list op type-tags))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                             (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic op a1 (t2->t1 a2)))
                            (else
                             (error "No method for these types"
                                    (list op type-tags)))))))
              (error "No method for there types"
                     (list op type-tags)))))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? 'scheme-number (lambda (x) (= x 0)))
  'done)
(install-scheme-number-package)
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; interal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (numer y) (denom x))))

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational)
       (lambda (x y) (and (= (numer x) (numer y))
                          (= (denom x) (denom y)))))
  (put '=zero? '(rational) (lambda (x)
                             (and (= (numer x) 0)
                                  (= (denom x) 0))))
  (put 'make 'rational
       (lambda (x y) (tag (make-rat x y))))
  'done)
(install-rational-package)
(define (make-rational n d)
  ((get 'make 'rational) n d))
(define r1 (make-rational 1 2))
(apply-generic 'add r1 r1)
(apply-generic 'sub r1 r1)
(apply-generic 'mul r1 r1)
(apply-generic 'div r1 r1)

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

(define (install-complex-package)
  (install-rectangular-package)
  (install-polar-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (real-part z)
    (apply-generic 'real-part z))
  (define (imag-part z)
    (apply-generic 'imag-part z))
  (define (magnitude z)
    (apply-generic 'magnitude z))
  (define (angle z)
    (apply-generic 'angle z))
  ;; internal procedures
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

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'complex x))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'equ? '(complex complex)
       (lambda (z1 z2) (and (= (real-part z1) (real-part z2))
                            (= (imag-part z1) (imag-part z2)))))
  (put '=zero? '(complex) (lambda (z) (and (= (real-part z) 0)
                                           (= (imag-part z) 0))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)
(install-complex-package)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
(let ((c1 (make-complex-from-real-imag 1 2))
      (c2 (make-complex-from-real-imag 3 4)))
  (newline)
  (display (apply-generic 'add c1 c2))
  (newline)
  (display (apply-generic 'sub c1 c2))
  (newline)
  (display (apply-generic 'mul c1 c2))
  (newline)
  (display (apply-generic 'div c1 c2))
  (newline)
  (display (apply-generic 'magnitude c1))
  'done)

(apply-generic 'equ? 1 2)
(apply-generic 'equ? 1 1)
(apply-generic 'equ?
               (make-complex-from-real-imag 1 2)
               (make-complex-from-real-imag 1 3))
(apply-generic 'equ?
               (make-complex-from-real-imag 1 2)
               (make-complex-from-real-imag 1 2))

(if (apply-generic '=zero?
                   (make-complex-from-real-imag 0 0))
    (display "等于0")
    (display "不等于0"))


(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)
((get-coercion 'scheme-number 'complex) 1)
(let ((c1 (make-complex-from-real-imag 1 1))
      (c2 (make-complex-from-real-imag 2 2)))
  (apply-generic 'add c1 c2))

(let ((c1 (make-complex-from-real-imag 1 1))
      (c2 1))
  (apply-generic 'add c1 c2))

(let ((c1 (make-rational 1 2))
      (c2 (make-rational 3 5)))
  (apply-generic 'add c1 c2))
(let ((c1 (make-rational 1 2))
      (c2 1))
  (apply-generic 'add c1 c2))

; 练习 2.81
(let ((c1 1)
      (c2 2))
  (display "练习 2.81")
  (newline)
  (apply-generic 'add c1 c2))

; 假设先加入一个将自身类型转换为自身的方法
(put-coercion 'scheme-number 'scheme-number (lambda (n) n))
; 先制造一个例子,两个类型相同时,依然会调用“强制”的类型转换过程
(let ((c1 (make-scheme-number 2))
      (c2 (make-scheme-number 3)))
  (display c1)
  (display c2)
  (apply-generic 'exp c1 c2))

; 练习 2.83
(let ((c1 (make-scheme-number 1.2))
      (c2 (make-complex-from-real-imag 1 2)))
  (apply-generic 'add c1 c2))

; 练习2.84
(define tower2.84 (list (list (cons 'a 'b) (lambda () (display "a->b") false))
                        (list (cons 'b 'c) (lambda () (display "b->c") false))
                        (list (cons 'c 'd) (lambda () (display "c->d") false))
                        (list (cons 'd 'e) (lambda () (display "d->e") false))))

; 提升类型到同一层
(define (type-tower type1 type2 tower)
  (if (eq? type1 type2)
      '()
      (let ((types-procedure (car tower)))
        (let ((types (car types-procedure))
              (procedure (cadr types-procedure)))
          (let ((t1 (car types))
                (t2 (cdr types))
                (left-tower (cdr tower)))
            (cond ((eq? type1 t1)
                   (append (list procedure)
                           (if (eq? type2 t2)
                               '()
                               (type-tower t2 type2 left-tower))))
                  ((eq? type2 t1)
                   (append (list procedure)
                           (if (eq? type1 t2)
                               '()
                               (type-tower t2 type1 left-tower))))
                  (else (type-tower type1 type2 left-tower))))))))

(let ((t1 'a)
      (t2 'b)
      (t3 'c))
  (let ((raise-list (type-tower t1 t3 tower2.84)))
    (map (lambda (x) (x)) raise-list)))

(let ((t1 'c)
      (t2 'a))
  (let ((raise-list (type-tower t1 t2 tower2.84)))
    (map (lambda (x) (x)) raise-list)))