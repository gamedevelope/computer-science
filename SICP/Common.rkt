#lang sicp
(#%provide apply-generic

           square
           cube
           echo
           gcd

           put-coercion
           put
           get
           get-coercion
           
           install-rational-package
           install-type-raise-package
           install-rectangular-package
           install-polar-package
           install-complex-package
           install-scheme-number-package
           
           make-table
           make-rational
           make-real
           make-complex-from-real-imag
           operation-table
           
           attach-tag
           type-tag
           contents

           rand-update)

(define (echo arg . args)
  (define (iter a b)
    (if (null? b)
        (display a)
        (begin
          (display a)
          (iter (car b) (cdr b)))))
  (iter arg args))

; 平方
(define (square x) (* x x))

; 立方
(define (cube x) (* x x x))

; gcd 求最大公约数
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; 随机数
(define (rand-update x)
  (remainder (round (/ (+ (* x 1103515245) 12345) 65536)) 32768))

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

(define (make-real n)
  (if (number? n)
      (attach-tag 'real n)
      (error "PARAM MUST BE NUMBER" n)))

(put 'equ? '(integer integer) (lambda (n1 n2) (eq? n1 n2)))
(put 'equ? '(real real) (lambda (n1 n2) (eq? n1 n2)))
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (cond ((integer? datum) 'integer)
        ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else
         (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((integer? datum) datum)
        ((real? datum) datum)
        ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else
         (error "Bad tagged datum -- CONTENTS" datum))))

(define (install-type-raise-package)
  (define (identity x) x)
  
  (define (type-tower type1 type2 tower)
    (if (eq? type1 type2)
        '()
        (let ((types-procedure (car tower))
              (left-tower (cdr tower)))
          (let ((types (car types-procedure))
                (procedure (cdr types-procedure)))
            (let ((t1 (car types))
                  (t2 (cdr types)))
              (cond ((eq? type1 t1)
                     (append (list (cons procedure identity))
                             (if (eq? type2 t2)
                                 '()
                                 (type-tower t2 type2 left-tower))))
                    ((eq? type2 t1)
                     (append (list (cons identity procedure))
                             (if (eq? type1 t2)
                                 '()
                                 (type-tower type1 t2 left-tower))))
                    (else (type-tower type1 type2 left-tower))))))))
  
  (define types (list (cons (cons 'integer 'rational)
                            (lambda (n) (make-rational n 1)))
                      (cons (cons 'rational 'real)
                            (lambda (n)
                              (make-real (/ ((get 'numer '(rational)) (contents n))
                                            ((get 'denom '(rational)) (contents n))))))
                      (cons (cons 'real 'complex)
                            (lambda (n) (make-complex-from-real-imag (contents n) 0)))))
  (define (raise-number n)
    (define (r n tower)
      (if (null? tower)
          n
          (let ((level (car tower)))
            (if (eq? (type-tag n) (caar level))
                ((cdr level) n)
                (r n (cdr tower))))))
    (r n types))
  (put 'raise 'number raise-number)
  
  (define (raise produres pair)
    (if (null? produres)
        pair
        (raise (cdr produres) (list ((caar produres) (car pair))
                                    ((cdar produres) (cadr pair))))))
  
  (define (raise-pair n1 n2)
    (let ((type1 (type-tag n1))
          (type2 (type-tag n2)))
      (let ((procedures (type-tower type1 type2 types)))
        (raise procedures (list n1 n2)))))
  
  (put-coercion 'raise
                'raise
                (lambda (n1 n2)
                  (raise-pair n1 n2)))
  'done)

(define (raise n1 n2)
  ((get-coercion 'raise 'raise) n1 n2))

(define (install-drop-package)
  (define real-part (get 'real-part '(complex)))
  (define type-tower (list (cons (cons 'complex 'real)
                                 (lambda (x)
                                   (make-real
                                    (real-part (contents x)))))
                           (cons (cons 'real 'rational)
                                 (lambda (x)
                                   (make-rational (round (contents x)) 1)))
                           (cons (cons 'rational 'integer)
                                 (lambda (x)
                                   (apply-generic 'numer x)))))
  (define (project n)
    (define (p n tower)
      (if (null? tower)
          n
          (let ((level (car tower)))
            (if (eq? (type-tag n) (caar level))
                ((cdr level) n)
                (p n (cdr tower))))))
    (p n type-tower))
  
  (define (drop n)
    (let ((dn (project n)))
      (cond ((eq? (type-tag n) (type-tag dn)) n) ; 类型相同,说明不可继续 drop 了
            ((apply-generic 'equ? n ((get 'raise 'number) dn)) (drop dn))
            (else n))))
  
  (put 'drop 'project project)
  (put 'drop 'number drop)
  "drop done")


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (if (eq? (type-tag (car args)) (type-tag (cadr args)))
                  (error "No method for these types" (list op type-tags))
                  (let ((raised-params (raise (car args) (cadr args))))
                    ((get 'drop 'number) (apply-generic op (car raised-params) (cadr raised-params)))))
              (error "No method for there types"
                     (list "op:" op "type-tags:" type-tags "args:" args)))))))

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
;
;(define (numer n)
;  ((get 'numer '(rational)) (contents n)))
;(define (denom n)
;  ((get 'denom '(rational)) n))

(define (make-rational n d)
  ((get 'make 'rational) n d))

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

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(install-rational-package)
(install-type-raise-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-scheme-number-package)
(install-drop-package)

(define drop (get 'drop 'number))
(define project (get 'drop 'project))
(define n (make-real 1))